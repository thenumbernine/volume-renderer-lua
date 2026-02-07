--[[
what should this do?
- custom underlying texture cpu type
- store its own cpu data vs accept from the user
--]]
local ffi = require 'ffi'
local class = require 'ext.class'
local assert = require 'ext.assert'
local vec3i = require 'vec-ffi.vec3i'
local gl = require 'gl'
local GLTex3D = require 'gl.tex3d'


local float = ffi.typeof'float'


local VolumeRenderer = class()

--[[
args:
	size = ctor for vec3i of size
	data = (optional) data of type to use, or float by default
	ctype = c type (TODO derive this from the internalFormat GL type)
		(TODO derive from data type?)
	internalFormat = GL internalFormat to use (TODO derive from ctype or from data?)
--]]
function VolumeRenderer:init(args)
	self.size = vec3i((assert.index(args, 'size')))
	self.volume = self.size:volume()
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
		self.data = ffi.new(ffi.typeof('$[?]', self.ctype), self.volume)
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
end


function VolumeRenderer:draw()
end


return VolumeRenderer
