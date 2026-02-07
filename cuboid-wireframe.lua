local ffi = require 'ffi'
local class = require 'ext.class'
local table = require 'ext.table'
local vec3f = require 'vec-ffi.vec3f'
local gl = require 'gl'
local GLSceneObject = require 'gl.sceneobject'


local cornerbits = function(i)
	return bit.band(i,1),
		bit.band(bit.rshift(i,1),1),
		bit.band(bit.rshift(i,2),1)
end


local CuboidWireframe = class()

--[[
args:
	view = for the mvProjMat
--]]
function CuboidWireframe:init(args)
	local vtxCPU = ffi.new'vec3f[8]'
	for i=0,7 do
		vtxCPU[i]:set(cornerbits(i))
	end

	local indexes = table()
	for i=0,7 do
		for j=0,2 do
			local k = bit.bxor(i, bit.lshift(1, j))
			if k > i then
				indexes:insert(i)
				indexes:insert(k)
			end
		end
	end

	self.globj = GLSceneObject{
		program = {
			version = 'latest',
			precision = 'best',
			vertexCode = [[
layout(location=0) in vec3 vertex;
uniform mat4 mvProjMat;
uniform vec3 mins, maxs;
void main() {
	gl_Position = mvProjMat * vec4(
		mix(mins, maxs, vertex),
		1.
	);
}
]],
			fragmentCode = [[
layout(location=0) out vec4 fragColor;
uniform vec4 color;

void main() {
	fragColor = color;
}
]],
			-- program's uniforms are only set upon init
			uniforms = {
				mins = args.mins or {-1,-1,-1},
				maxs = args.maxs or {1,1,1},
				color = args.color or {1,1,1,1},
			},
		},
		vertexes = {
			dim = 3,
			data = vtxCPU,
			size = ffi.sizeof(vtxCPU),
		},
		geometry = {
			mode = gl.GL_LINES,
			indexes = {data=indexes},
		},
		-- sceneobj's uniforms update every frame
		uniforms = {
			mvProjMat = args.view.mvProjMat.ptr,
		},
	}
end

function CuboidWireframe:draw()
	self.globj:draw()
end

return CuboidWireframe 
