local ffi = require 'ffi'
local class = require 'ext.class'
local vec3f = require 'vec-ffi.vec3f'
local vector = require 'ffi.cpp.vector-lua'
local gl = require 'gl'
local GLSceneObject = require 'gl.sceneobject'


local CuboidWireframe = class()

--[[
args:
	view = for the mvProjMat
--]]
function CuboidWireframe:init(args)
	self.lineVtxCPU = vector(vec3f)
	local cornerbits = function(i)
		return bit.band(i,1),
			bit.band(bit.rshift(i,1),1),
			bit.band(bit.rshift(i,2),1)
	end
	for i=0,7 do
		for j=0,2 do
			local k = bit.bxor(i, bit.lshift(1, j))
			if k > i then
				self.lineVtxCPU:emplace_back()[0]:set(cornerbits(i))
				self.lineVtxCPU:emplace_back()[0]:set(cornerbits(k))
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
			-- program's uniforms are only set upon init
			uniforms = {
				volumeMin = {-1,-1,-1},
				volumeMax = {1,1,1},
			},
		},
		vertexes = {
			dim = 3,
			data = self.lineVtxCPU.v,
			size = self.lineVtxCPU:getNumBytes(),
		},
		geometry = {
			mode = gl.GL_LINES,
			count = #self.lineVtxCPU,
		},
		-- sceneobj's uniforms update every frame
		uniforms = {
			mvProjMat = args.view.mvProjMat.ptr,
			volumeMin = args.volumeMin or nil,
			volumeMax = args.volumeMax or nil,
		},
	}
end

function CuboidWireframe:draw()
	self.lineObj:draw()
end

return CuboidWireframe 
