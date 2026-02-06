#!/usr/bin/env luajit
--[[
TODO make an invokable class with this, similar to force-directed-graph, plot2d, plot3d, etc
esp so I can put my raytracer / slicer in one place
esp since now I've got earthquake-shear-lines/viz-hist-splat-volume.lua doing it

test demo here: mandelbrot set

another good one: the hydrogen wavefunction visualizer i've already got in webgl

another good one / thing to integrate: hydro-cl
--]]
local ffi = require 'ffi'
local table = require 'ext.table'
local range = require 'ext.range'
local cmdline = require 'ext.cmdline'(...)
local vec2f = require 'vec-ffi.vec2f'
local vec3i = require 'vec-ffi.vec3i'
local vec3f = require 'vec-ffi.vec3f'
local box3f = require 'vec-ffi.box3f'
local vector = require 'ffi.cpp.vector-lua'
local gl = require 'gl.setup'(cmdline.gl)
local glnumber = require 'gl.number'
local GLTex2D = require 'gl.tex2d'
local GLSceneObject = require 'gl.sceneobject'
local GLHSVTex2D = require 'gl.hsvtex2d'
local GLArrayBuffer = require 'gl.arraybuffer'
local GLTex3D = require 'gl.tex3d'
local ig = require 'imgui'



local densitySize = vec3i(64, 64, 64) 

local densityData = ffi.new('float[?]', densitySize:volume())

local vars = {
	useLog = true,
	mandelConst = -.5,
	mandelPerm = 0,	-- 0-3
}

local logDensityEpsilon = 1e-5

local densityRange = vec2f()
local logDensityRange = vec2f()

local function recalc()
	local b = box3f({-2, -2, -2}, {2, 2, 2})
	local ptr = densityData+0
	local fw = vars.mandelConst
	local perm = vars.mandelPerm
print('perm', perm)	
	densityRange:set(math.huge, -math.huge)
	for k=0,densitySize.z-1 do
		local fz = (k+.5)/tonumber(densitySize.z) * (b.max.z - b.min.z) + b.min.z
		for j=0,densitySize.y-1 do
			local fy = (j+.5)/tonumber(densitySize.y) * (b.max.y - b.min.y) + b.min.y
			for i=0,densitySize.x-1 do
				local fx = (i+.5)/tonumber(densitySize.x) * (b.max.x - b.min.x) + b.min.x
				
				-- mandelbrot init is z={0,0}, c={x,y}
				-- julia init is z={x,y}, c=fixed
				local zr, zi, cr, ci = 0,0,0,0
				if perm == 0 then
					zr, zi, cr, ci = fw, fx, fy, fz
				elseif perm == 1 then
					zr, zi, cr, ci = fz, fw, fx, fy
				elseif perm == 2 then
					zr, zi, cr, ci = fy, fz, fw, fx
				elseif perm == 3 then
					zr, zi, cr, ci = fx, fy, fz, fw
				end
	
				for iter = 0,20 do
					zr, zi = zr*zr - zi*zi + cr, 2*zr*zi + ci
					local zn = zr*zr + zi*zi
					if zn > 4 then
						ptr[0] = iter
						densityRange.x = math.min(densityRange.x, iter)
						densityRange.y = math.max(densityRange.y, iter)
						break
					end
				end

				ptr=ptr+1
			end
		end
	end

	print('density range', densityRange)
end


_G.densityAlphaRange = vec3f(2, 0, 4)	-- .x = period, .y = offset, .z = gamma

-- range in 3D space
_G.volumeRange = box3f(
	vec3f(-1, -1, -1),
	vec3f(1, 1, 1)
)


recalc()

local App = require 'imgui.appwithorbit'()

function App:initGL()
	App.super.initGL(self)

-- [=[
	_G.lineVtxCPU = vector(vec3f)
	local cornerbits = function(i)
		return bit.band(i,1),
			bit.band(bit.rshift(i,1),1),
			bit.band(bit.rshift(i,2),1)
	end
	for i=0,7 do
		for j=0,2 do
			local k = bit.bxor(i, bit.lshift(1, j))
			if k > i then
				lineVtxCPU:emplace_back()[0]:set(cornerbits(i))
				lineVtxCPU:emplace_back()[0]:set(cornerbits(k))
			end
		end
	end
	self.lineObj = GLSceneObject{
		program = {
			version = 'latest',
			precision = 'best',
			vertexCode = [[
layout(location=0) in vec3 vertex;
uniform mat4 mvProjMat;
uniform vec3 volumeMin, volumeMax;
void main() {
	gl_Position = mvProjMat * vec4(
		mix(volumeMin, volumeMax, vertex),
		1.
	);
}
]],
			fragmentCode = [[
layout(location=0) out vec4 fragColor;

void main() {
	fragColor = vec4(.5, .5, .5, 1.);
}
]],
		},
		vertexes = {
			dim = 3,
			data = lineVtxCPU.v,
			size = lineVtxCPU:getNumBytes(),
		},
		geometry = {
			mode = gl.GL_LINES,
			count = #lineVtxCPU,
		},
	}
--]=]


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

		local sliceRes = densitySize.s[axis]
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
--]=]

-- [=[
	self.volumeTex = GLTex3D{
		width = densitySize.x,
		height = densitySize.y,
		depth = densitySize.z,
		internalFormat = gl.GL_R32F,
		data = densityData,
		--[[ mipmapping messes with transparency across slices so ....
		magFilter = gl.GL_LINEAR,
		minFilter = gl.GL_LINEAR_MIPMAP_LINEAR,
		generateMipmap = true,
		--]]
		-- [[
		magFilter = gl.GL_NEAREST,
		minFilter = gl.GL_NEAREST,
		--]]
		wrap = {
			s = gl.GL_CLAMP_TO_EDGE,
			t = gl.GL_CLAMP_TO_EDGE,
		},
	}:unbind()

--[[ how to swap after-the-fact ?
	self.volumeObj.vertexes = self.vtxGPUsPerSide[1]
	self.volumeObj.attrs.vertex.buffer = self.volumeObj.vertexes
	self.volumeObj.geometry.count = #self.volumeObj.vertexes.vec
--]]

	local volumeVtxGPU = self.vtxGPUsPerSide[1]
	self.volumeObj = GLSceneObject{
		program = {
			version = 'latest',
			precision = 'best',
			vertexCode = [[
layout(location=0) in vec3 vertex;
layout(location=0) out vec3 tc;

uniform mat4 mvProjMat;
uniform vec3 volumeMin, volumeMax;

void main() {
	tc = vertex;
	gl_Position = mvProjMat * vec4(
		mix(volumeMin, volumeMax, vertex),
		1.
	);
}
]],
			fragmentCode = [[
precision highp sampler3D;
layout(location=0) in vec3 tc;
layout(location=0) out vec4 fragColor;

uniform sampler3D volumeTex;
uniform sampler2D hsvTex;
uniform vec2 densityRange;
uniform vec3 densityAlphaRange;	// x = freq, y = offset, z = gamma
uniform bool useLog;

#define logDensityEpsilon ]]..glnumber(logDensityEpsilon)..[[ 

float fpart(float x) {
	return x - floor(x);
}

void main() {
	float density = texture(volumeTex, tc).r;
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
				volumeTex = 0,
				hsvTex = 1,
			},
		},
		geometry = {
			mode = gl.GL_TRIANGLES,
			count = #volumeVtxGPU.vec,
		},
		vertexes = volumeVtxGPU,
		texs = {
			self.volumeTex,
			self.hsvTex,
		},
	}
--]=]

	gl.glEnable(gl.GL_DEPTH_TEST)
	gl.glDepthFunc(gl.GL_LEQUAL)
	gl.glBlendFunc(gl.GL_SRC_ALPHA, gl.GL_ONE_MINUS_SRC_ALPHA)
end

function App:update()
	gl.glClear(bit.bor(gl.GL_COLOR_BUFFER_BIT, gl.GL_DEPTH_BUFFER_BIT))

	if self.lineObj then
		self.lineObj.uniforms.mvProjMat = self.view.mvProjMat.ptr
		self.lineObj.uniforms.volumeMin = volumeRange.min.s
		self.lineObj.uniforms.volumeMax = volumeRange.max.s
		self.lineObj:draw()
	end

	local volumeObj = self.volumeObj
	if volumeObj then
		local fwd = self.view.angle:zAxis()
		local absfwd = fwd:map(math.abs)
		local maxdir = select(2, table.sup{absfwd:unpack()})-1
		local vtxGPUIndex = maxdir + (fwd.s[maxdir] > 0 and 3 or 0)
		local vtxGPU = self.vtxGPUsPerSide[1+vtxGPUIndex]
		local lastVtxGPU = volumeObj.vertexes
		if vtxGPU ~= lastVtxGPU then
			volumeObj.vertexes = vtxGPU
			volumeObj.attrs.vertex.buffer = vtxGPU
			volumeObj.geometry.vertexes = vtxGPU
			volumeObj.geometry.count = #vtxGPU.vec

			-- weird interchange between GLSceneObject's .attrs (keyed by name, contains vao's ctor args)
			--  and GLVertexArray's .attrs (keyed by index)
			select(2, volumeObj.vao.attrs:find(nil, function(attr)
				return attr.name == 'vertex'
			end)).buffer = vtxGPU
			volumeObj.vao:setAttrs()
		end

		gl.glDepthMask(gl.GL_FALSE)
		gl.glEnable(gl.GL_BLEND)
		volumeObj.uniforms.mvProjMat = self.view.mvProjMat.ptr
		if vars.useLog then
			logDensityRange.x = math.log(densityRange.x + logDensityEpsilon) 
			logDensityRange.y = math.log(densityRange.y + logDensityEpsilon) 
		end
		volumeObj.uniforms.densityRange = vars.useLog and logDensityRange.s or densityRange.s
		volumeObj.uniforms.densityAlphaRange = densityAlphaRange.s
		volumeObj.uniforms.volumeMin = volumeRange.min.s
		volumeObj.uniforms.volumeMax = volumeRange.max.s
		volumeObj.uniforms.useLog = vars.useLog
		volumeObj:draw()
		gl.glDisable(gl.GL_BLEND)
		gl.glDepthMask(gl.GL_TRUE)
	end

	App.super.update(self)
end

function App:updateGUI()
	if ig.igBeginMainMenuBar() then
		if ig.igBeginMenu'menu' then

			ig.luatableCheckbox('ortho', self.view, 'ortho')
			if ig.igButton'reset view' then
				self.view.ortho = true
				self.view.orthoSize = self.viewOrthoSize
				self.view.angle:set(0,0,0,1)
				self.view.orbit:set(0,0,0)
				self.view.pos:set(0,0,self.viewDist)
			end

			ig.igInputFloat('densityMin', densityRange.s + 0)
			ig.igInputFloat('densityMax', densityRange.s + 1)

			-- logDensity is normalized to this range [.x,.y], and then clamped to (0,1), and then scaled by .z
			ig.igInputFloat('densityAlphaPeriod', densityAlphaRange.s + 0)
			ig.igSliderFloat('densityAlphaOffset', densityAlphaRange.s + 1, 0, 1)
			ig.igInputFloat('densityAlphaGamma', densityAlphaRange.s + 2)

			if ig.luatableInputFloat('mandelConst', vars, 'mandelConst') then
				recalc()
			end
			if ig.luatableInputInt('mandelPerm', vars, 'mandelPerm') then
				recalc()
			end

			ig.luatableCheckbox('useLog', vars, 'useLog')
	
--[[
			ig.igInputFloat('volumeMinX', volumeRange.min.s + 0)
			ig.igInputFloat('volumeMinY', volumeRange.min.s + 1)
			ig.igInputFloat('volumeMinZ', volumeRange.min.s + 2)

			ig.igInputFloat('volumeMaxX', volumeRange.max.s + 0)
			ig.igInputFloat('volumeMaxY', volumeRange.max.s + 1)
			ig.igInputFloat('volumeMaxZ', volumeRange.max.s + 2)
--]]
			ig.igEndMenu()
		end
		ig.igEndMainMenuBar()
	end
end

return App():run()
