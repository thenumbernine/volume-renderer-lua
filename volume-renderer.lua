--[[
what should this do?
- custom underlying texture cpu type
- store its own cpu data vs accept from the user
--]]
local ffi = require 'ffi'
local class = require 'ext.class'
local assert = require 'ext.assert'
local range = require 'ext.range'
local table = require 'ext.table'
local vec2f = require 'vec-ffi.vec2f'
local vec3f = require 'vec-ffi.vec3f'
local vec3i = require 'vec-ffi.vec3i'
local gl = require 'gl'
local GLTex3D = require 'gl.tex3d'
local GLHSVTex2D = require 'gl.hsvtex2d'
local GLSceneObject = require 'gl.sceneobject'
local GLArrayBuffer = require 'gl.arraybuffer'
local glnumber = require 'gl.number'


local float = ffi.typeof'float'


local logDensityEpsilon = 1e-5

local VolumeRenderer = class()


-- whether to use log-scaling
VolumeRenderer.useLog = false


--[[
args:
	size = ctor for vec3i of size
	data = (optional) data of type to use, or float by default
	ctype = c type (TODO derive this from the internalFormat GL type)
		(TODO derive from data type?)
	internalFormat = GL internalFormat to use (TODO derive from ctype or from data?)
	view = where to get mvProjMat from
	mins = (optional) bbox min
	maxs = (optional) bbox max
	useLog
--]]
function VolumeRenderer:init(args)
	self.size = vec3i((assert.index(args, 'size')))

	self.data = args.data
	if args.ctype then
		self.ctype = ffi.typeof(args.ctype)
	elseif args.data then
		-- ffi can go from base to ptr type
		-- but can it go from ptr to base type?
		self.ctype = ffi.typeof(
			tostring(ffi.typeof(self.data+0)):match'^ctype<(.*) %*>$'
		)
	else
		-- no data and no ctype?
		-- should we query for args.internalFormat or just use float?
		self.ctype = float
	end
print('ctype', self.ctype)

	-- if we weren't given data then allocate it
	if not self.data then
print'allocating new data'
		self.data = ffi.new(ffi.typeof('$[?]', self.ctype), self.size:volume())
		self.owns = true	-- does it matter?  meh?
	end

	self.internalFormat = args.internalFormat
	if not self.internalFormat then
		-- self.internalFormat
		-- 1) find GL type for ctype
		local glType = assert.index(
			require 'gl.types'.gltypeForCType,
			tostring(self.ctype),
			'gl.types.gltypeForCType'
		)
		-- 2) find internalFormat for GL type
		-- TODO save names in gl.tex.formatInfos, for easier error reporting?
		local formatInfo = select(2, GLTex3D.formatInfos:find(nil, function(info)
			return info.types
			and info.types:find(glType)
			-- how do you make sure you're getting a color texture and not a depth texture...
			-- TODO only on the GLES entries do I have "internalComponents" as a table of:
			--  R G B L A for color/lum/alpha
			--  D for depth
			--  S for stencil
			-- ... should I complete this for all entries? where is this info?
			-- for the subsequent entries there's a list of *Bits per field that it uses:
			and (
				-- if you don't do this then you'll get a float16 for float32 data...
				(
					(info.redBits or 0)
					+ (info.greenBits or 0)
					+ (info.blueBits or 0)
					+ (info.alphaBits or 0)
				) == 8*ffi.sizeof(self.ctype)
			)
			and not depthBits
			and not stencilBits	
		end)) or error("failed to find tex internalFormat for glType "..tostring(glType))
		self.internalFormat = formatInfo.internalFormat
	end


	self.useLog = args.useLog
	self.view = assert.index(args, 'view')

	-- densityRange holds the density range
	-- logDensityRange holds the log-mapped density range
	-- either is used as a uniform depending on whether useLog is set
	self.densityRange = vec2f()

	self.logDensityRange = vec2f()


	-- .x = period, .y = offset, .z = gamma
	self.densityAlphaRange = vec3f(2, 0, 4)	



	-- now that we're done parsing args


	self.tex = GLTex3D{
		width = self.size.x,
		height = self.size.y,
		depth = self.size.z,
		internalFormat = self.internalFormat,
		data = self.data,
		magFilter = gl.GL_NEAREST,
		minFilter = gl.GL_NEAREST,
		wrap = {
			s = gl.GL_CLAMP_TO_EDGE,
			t = gl.GL_CLAMP_TO_EDGE,
		},
	}:unbind()


	self.hsvTex = GLHSVTex2D(256, nil, true)
		:unbind()

	self.vtxGPUsPerSide = range(0,5):mapi(function(side)
		local axis = side % 3
		local pm = side >= 3

		local k = axis
		local k2 = (axis + 1) % 3
		local k3 = (axis + 2) % 3
		local function set(v, x,y,z)
			v.s[k2] = x
			v.s[k3] = y
			v.s[k] = z
		end

		local sliceRes = self.size.s[axis]
		local vtxGPU = GLArrayBuffer{
			dim = 3,
			useVec = true,
		}
		local vtxCPU = vtxGPU:beginUpdate()
		local imin, imax, istep
		if pm then
			imin, imax, istep = 0, sliceRes-1, 1
		else
			imin, imax, istep = sliceRes-1, 0, -1
		end
		for i=imin,imax,istep do
			local f = (i + .5) / sliceRes
			set(vtxCPU:emplace_back(), 0, 0, f)
			set(vtxCPU:emplace_back(), 1, 0, f)
			set(vtxCPU:emplace_back(), 0, 1, f)
			set(vtxCPU:emplace_back(), 1, 0, f)
			set(vtxCPU:emplace_back(), 0, 1, f)
			set(vtxCPU:emplace_back(), 1, 1, f)
		end
		vtxGPU:endUpdate()
		vtxGPU:unbind()
		return vtxGPU
	end)


--[[ how to swap after-the-fact ?
	self.globj.vertexes = self.vtxGPUsPerSide[1]
	self.globj.attrs.vertex.buffer = self.globj.vertexes
	self.globj.geometry.count = #self.globj.vertexes.vec
--]]

	local volumeVtxGPU = self.vtxGPUsPerSide[1]
	self.globj = GLSceneObject{
		program = {
			version = 'latest',
			precision = 'best',
			vertexCode = [[
layout(location=0) in vec3 vertex;
layout(location=0) out vec3 tc;

uniform mat4 mvProjMat;
uniform vec3 mins, maxs;

void main() {
	tc = vertex;
	gl_Position = mvProjMat * vec4(
		mix(mins, maxs, vertex),
		1.
	);
}
]],
			fragmentCode = [[
precision highp sampler3D;
layout(location=0) in vec3 tc;
layout(location=0) out vec4 fragColor;

uniform sampler3D tex;
uniform sampler2D hsvTex;
uniform vec2 densityRange;
uniform vec3 densityAlphaRange;	// x = freq, y = offset, z = gamma
uniform bool useLog;

#define logDensityEpsilon ]]..glnumber(logDensityEpsilon)..[[ 

float fpart(float x) {
	return x - floor(x);
}

void main() {
	float density = texture(tex, tc).r;
	if (useLog) {
		density = log(density + logDensityEpsilon);
	}
	float gradLookup = (density - densityRange.x) / (densityRange.y - densityRange.x);

	fragColor = texture(hsvTex, vec2(gradLookup, .5));
	fragColor.a = pow(
		fpart(density / densityAlphaRange.x + densityAlphaRange.y),
		densityAlphaRange.z
	);
}
]],
			uniforms = {
				tex = 0,
				hsvTex = 1,
				-- upload on init
				mins = args.mins or {-1,-1,-1},
				maxs = args.maxs or {1,1,1},
				useLog = self.useLog,
			},
		},
		geometry = {
			mode = gl.GL_TRIANGLES,
			count = #volumeVtxGPU.vec,
		},
		vertexes = volumeVtxGPU,
		texs = {
			self.tex,
			self.hsvTex,
		},
		-- GLSceneObject uniforms update every frame:
		uniforms = {
			densityAlphaRange = self.densityAlphaRange.s,
			mvProjMat = self.view.mvProjMat.ptr,
		},
	}
end

function VolumeRenderer:draw()
	local fwd = self.view.angle:zAxis()
	local absfwd = fwd:map(math.abs)
	local maxdir = select(2, table.sup{absfwd:unpack()})-1
	local vtxGPUIndex = maxdir + (fwd.s[maxdir] > 0 and 3 or 0)
	local vtxGPU = self.vtxGPUsPerSide[1+vtxGPUIndex]
	local lastVtxGPU = self.globj.vertexes
	if vtxGPU ~= lastVtxGPU then
		self.globj.vertexes = vtxGPU
		self.globj.attrs.vertex.buffer = vtxGPU
		self.globj.geometry.vertexes = vtxGPU
		self.globj.geometry.count = #vtxGPU.vec

		-- weird interchange between GLSceneObject's .attrs (keyed by name, contains vao's ctor args)
		--  and GLVertexArray's .attrs (keyed by index)
		select(2, self.globj.vao.attrs:find(nil, function(attr)
			return attr.name == 'vertex'
		end)).buffer = vtxGPU
		self.globj.vao:setAttrs()
	end

	gl.glDepthMask(gl.GL_FALSE)
	gl.glEnable(gl.GL_BLEND)
	gl.glBlendFunc(gl.GL_SRC_ALPHA, gl.GL_ONE_MINUS_SRC_ALPHA)
	
	if self.useLog then
		self.logDensityRange.x = math.log(self.densityRange.x + logDensityEpsilon) 
		self.logDensityRange.y = math.log(self.densityRange.y + logDensityEpsilon) 
		self.globj.uniforms.densityRange = self.logDensityRange.s
	else
		self.globj.uniforms.densityRange = self.densityRange.s
	end

	self.globj:draw()
	gl.glDisable(gl.GL_BLEND)
	gl.glDepthMask(gl.GL_TRUE)
end

return VolumeRenderer
