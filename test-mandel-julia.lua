#!/usr/bin/env luajit
local ffi = require 'ffi'
local template = require 'template'
local assert = require 'ext.assert'
local range = require 'ext.range'
local cmdline = require 'ext.cmdline'(...)
local vec3i = require 'vec-ffi.vec3i'
local box3f = require 'vec-ffi.box3f'
local gl = require 'gl.setup'(cmdline.gl)
local GLProgram = require 'gl.program'
local ig = require 'imgui'
local CuboidWireframe = require 'volume-renderer.cuboid-wireframe'
local VolumeRenderer = require 'volume-renderer'


local App = require 'imgui.appwithorbit'()

App.viewDist = 3

function App:initGL()
	App.super.initGL(self)

	self.mandelMaxIter = 20
	self.mandelConst = -.5
	self.mandelPerm = 2	-- 0-3
	self.mandelEscapeNormSq = 4
	self.autoUpdateValueRange = false	-- done on cpu so it goes slow
	self.recalcWithCompute = not cmdline.recalcWithCPU

	self.valueCPUDirty = nil -- true means GPU needs update to CPU contents
	self.valueGPUDirty = nil -- true means CPU needs update to GPU contents

	-- set to true to update valueRangeDirty from valueCPU
	self.valueRangeDirty = nil

	-- range in 3D space
	self.worldBBox = box3f(
		{-1, -1, -1},
		{1, 1, 1}
	)

	self.cuboidWireframe = CuboidWireframe{
		view = self.view,
		color = {.5, .5, .5, 1},
	}
	-- set it to update every frame
	self.cuboidWireframe.globj.uniforms.mins = self.worldBBox.min.s
	self.cuboidWireframe.globj.uniforms.maxs = self.worldBBox.max.s


	self.vol = VolumeRenderer{
		view = self.view,
		--size = {4,4,4},
		size = {256, 256, 256},
		--size = {512, 512, 512},
		--useLog = true,
	}
	self.vol.valueRange:set(0, self.mandelMaxIter)

	-- set it to update every frame
	self.vol.globj.uniforms.mins = self.worldBBox.min.s
	self.vol.globj.uniforms.maxs = self.worldBBox.max.s

	self.graphBBox = box3f({-1, -1, -1}, {1, 1, 1})



	-- [[ all of this is compute-shader-redundant and should go in its own function somewhere


	-- prepare our mandelbrot calc compute shader

	-- can I require this at global scope, or will it try to access the gl. lib namespace and screw up on Windows?
	local GLGlobal = require 'gl.global'

	-- each global size dim must be <= this
	local maxComputeWorkGroupCount = vec3i(GLGlobal:get'GL_MAX_COMPUTE_WORK_GROUP_COUNT')
	print('GL_MAX_COMPUTE_WORK_GROUP_COUNT = '..maxComputeWorkGroupCount)

	-- each local size dim must be <= this
	local maxComputeWorkGroupSize = vec3i(GLGlobal:get'GL_MAX_COMPUTE_WORK_GROUP_SIZE')
	print('GL_MAX_COMPUTE_WORK_GROUP_SIZE = '..maxComputeWorkGroupSize)

	-- the product of all local size dims must be <= this
	-- also, this is >= 1024
	local maxComputeWorkGroupInvocations = GLGlobal:get'GL_MAX_COMPUTE_WORK_GROUP_INVOCATIONS'
	print('GL_MAX_COMPUTE_WORK_GROUP_INVOCATIONS = '..maxComputeWorkGroupInvocations)

	local function roundPow2(x)
		-- lazy way to round to nearest power-of-two
		return 2^math.floor(math.log(x, 2))
	end

	print('computeSize', self.vol.size)

	-- product must be <= GL_MAX_COMPUTE_WORK_GROUP_INVOCATIONS
	self.computeLocalSize = vec3i()
	self.computeLocalSize.x = math.min(
		self.vol.size.x,
		roundPow2(tonumber(maxComputeWorkGroupInvocations)^(1/3))
	)
	self.computeLocalSize.y = math.min(
		self.vol.size.y,
		roundPow2(math.sqrt(tonumber(maxComputeWorkGroupInvocations / self.computeLocalSize.x)))
	)
	self.computeLocalSize.z = math.min(
		self.vol.size.z,
		maxComputeWorkGroupInvocations / (self.computeLocalSize.x * self.computeLocalSize.y)
	)
	print('computeLocalSize', self.computeLocalSize)

	self.computeNumGroups = self.vol.size / self.computeLocalSize
	print('computeNumGroups', self.computeNumGroups)
	-- TODO this assertion is not necessary, but if it fails that means I will have to add bounds-checking of the index to the kernels.
	assert.eq(self.computeNumGroups:elemMul(self.computeLocalSize), self.vol.size)


	-- ]]



	self.computeShader = GLProgram{
		version = 'latest',
		precision = 'best',
		computeCode = template([[
layout(local_size_x=<?=computeLocalSize.x
	?>, local_size_y=<?=computeLocalSize.y
	?>, local_size_z=<?=computeLocalSize.z
	?>) in;

layout(<?=texFormat?>, binding=0) uniform writeonly image3D dstTex;

uniform vec3 graphMin;
uniform vec3 graphMax;
uniform float mandelMaxIter;
uniform double mandelConst;
uniform double mandelEscapeNormSq;
uniform int mandelPerm;

void main() {
	ivec3 itc = ivec3(gl_GlobalInvocationID.xyz);

	vec3 texSize = vec3(256, 256, 256);
	dvec4 f = dvec4(
		mix(graphMin, graphMax, vec3(itc) / texSize),
		mandelConst
	);
	dvec2 z, c;

	if (mandelPerm == 0) {
		z = dvec2(f.w, f.x);
		c = dvec2(f.y, f.z);
	} else if (mandelPerm == 1) {
		z = dvec2(f.z, f.w);
		c = dvec2(f.x, f.y);
	} else if (mandelPerm == 2) {
		z = dvec2(f.y, f.z);
		c = dvec2(f.w, f.x);
	} else if (mandelPerm == 3) {
		z = dvec2(f.x, f.y);
		c = dvec2(f.z, f.w);
	}

	float iter = 0.;
	for (; iter < mandelMaxIter; iter += 1.) {
		z = dvec2(
			z.x*z.x - z.y*z.y + c.x,
			2.*z.x*z.y + c.y
		);
		double zn = dot(z,z);
		if (zn > mandelEscapeNormSq) break;
	}

	imageStore(dstTex, itc, vec4(iter, 0., 0., 0.));
}
]], 	{
			computeLocalSize = self.computeLocalSize,
			texFormat = self.vol.tex:getFormatInfo().glslFormatName,
		}),
	}
		:bindImage(
			0,					-- binding
			self.vol.tex,		-- tex
			gl.GL_WRITE_ONLY	-- read/write
		)
		:useNone()

	self:recalc()

	gl.glEnable(gl.GL_DEPTH_TEST)
end

function App:recalc()
	if self.recalcWithCompute then
		self:recalcGPU()
	else
		self:recalcCPU()
	end
end

function App:recalcCPU()
	local b = self.graphBBox
	local ptr = self.vol.data+0
	local fw = self.mandelConst
	local mandelPerm = self.mandelPerm
	local mandelEscapeNormSq = self.mandelEscapeNormSq
	local size = self.vol.size
	for k=0,size.z-1 do
		local fz = (k+.5)/tonumber(size.z) * (b.max.z - b.min.z) + b.min.z
		for j=0,size.y-1 do
			local fy = (j+.5)/tonumber(size.y) * (b.max.y - b.min.y) + b.min.y
			for i=0,size.x-1 do
				local fx = (i+.5)/tonumber(size.x) * (b.max.x - b.min.x) + b.min.x

				-- mandelbrot init is z={0,0}, c={x,y}
				-- julia init is z={x,y}, c=fixed
				local zr, zi, cr, ci = 0,0,0,0
				if mandelPerm == 0 then
					zr, zi, cr, ci = fw, fx, fy, fz
				elseif mandelPerm == 1 then
					zr, zi, cr, ci = fz, fw, fx, fy
				elseif mandelPerm == 2 then
					zr, zi, cr, ci = fy, fz, fw, fx
				elseif mandelPerm == 3 then
					zr, zi, cr, ci = fx, fy, fz, fw
				end

				for iter=0,self.mandelMaxIter do
					zr, zi = zr*zr - zi*zi + cr, 2*zr*zi + ci
					local zn = zr*zr + zi*zi
					if zn > mandelEscapeNormSq then
						ptr[0] = iter
						break
					end
				end

				ptr=ptr+1
			end
		end
	end

	self.valueGPUDirty = true	-- GPU is behind
	self.valueRangeDirty = true	-- valueRange is behind
end

function App:recalcGPU()
	self.computeShader
		:use()
		:setUniforms{
			graphMin = self.graphBBox.min.s,
			graphMax = self.graphBBox.max.s,
			mandelMaxIter = self.mandelMaxIter,
			mandelConst = self.mandelConst,
			mandelEscapeNormSq = self.mandelEscapeNormSq,
			mandelPerm = self.mandelPerm,
		}
		:dispatchCompute(self.computeNumGroups:unpack())

	-- should memory barrier be a GLProgram function or nah?
	gl.glMemoryBarrier(gl.GL_SHADER_IMAGE_ACCESS_BARRIER_BIT)

	self.computeShader:useNone()

	self.valueCPUDirty = true
	self.valueRangeDirty = true
end

function App:flushValueCPUToGPU()
	-- if GPU data is old then update it from CPU data
	if not self.valueGPUDirty then return end
print('flushValueCPUToGPU')
	assert(not self.valueCPUDirty)
	self.vol.tex
		:bind()
		:subimage()
		:unbind()
	self.valueGPUDirty = nil
end

function App:flushValueGPUToCPU()
	if not self.valueCPUDirty then return end
print('flushValueGPUToCPU')
	assert(not self.valueGPUDirty)
	local tex = self.vol.tex
	tex:bind()
		:getImage()
		:unbind()
	self.valueCPUDirty = nil
end

function App:update()
	gl.glClear(bit.bor(gl.GL_COLOR_BUFFER_BIT, gl.GL_DEPTH_BUFFER_BIT))

	-- see if we should update our valueRange
	if self.autoUpdateValueRange 	-- user requests update
	and self.valueRangeDirty	-- values have been modified elsewhere
	then
		self:flushValueGPUToCPU()	-- do our reduce on the CPu, so make sure CPU is up to date
print('flushValueRange')

		-- [[ update the range data
		-- this could be done much faster as a reduce kernel
		-- I'm already doing it this way in my hydro-cl project
		local valueRange = self.vol.valueRange
		valueRange:set(math.huge, -math.huge)
		local ptr = self.vol.data+0
		for i=0,self.vol.size:volume()-1 do
			valueRange.x = math.min(valueRange.x, ptr[0])
			valueRange.y = math.max(valueRange.y, ptr[0])
			ptr=ptr+1
		end
		--]]

		self.valueRangeDirty = nil
	end

	-- see if we should update our GPU data before we draw
	self:flushValueCPUToGPU()


	self.cuboidWireframe:draw()

	self.vol:draw()


	-- do mouse ray -> intersect with self.worldBBox
	-- click to recenter on axis of side of click
	-- click+drag to change bounds (TODO maintain aspect ratio?)
	--print(self.mouse.pos)

	if self.mouse.rightDown then
		if self.mouse.deltaPos.x ~= 0
		or self.mouse.deltaPos.y ~= 0
		then
			if self.leftShiftDown
			or self.rightShiftDown
			then
				-- zoom volume
				local dy = self.mouse.deltaPos.y
				local center = self.graphBBox:center()
				local newHalfSize = .5 * self.graphBBox:size() * math.exp(10 * dy)
				self.graphBBox.max = center + newHalfSize
				self.graphBBox.min = center - newHalfSize
				self:recalc()
			else
				-- drag volume
				local delta = (
					  self.view.angle:xAxis() * self.mouse.deltaPos.x
					+ self.view.angle:yAxis() * self.mouse.deltaPos.y
				) * self.view.pos:length()

				-- ... divide by volume 3D size
				-- ... times by graph size
				local graphSize = self.graphBBox:size()
				local worldSize = self.worldBBox:size()
				delta.x = delta.x * graphSize.x / worldSize.x
				delta.y = delta.y * graphSize.y / worldSize.y
				delta.z = delta.z * graphSize.z / worldSize.z
				-- and now scroll in our graph bounds
				self.graphBBox.min = self.graphBBox.min - delta
				self.graphBBox.max = self.graphBBox.max - delta
				self:recalc()
			end
		end
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

			if ig.igInputFloat('graph minX', self.graphBBox.min.s + 0)
			or ig.igInputFloat('graph minY', self.graphBBox.min.s + 1)
			or ig.igInputFloat('graph minZ', self.graphBBox.min.s + 2)
			or ig.igInputFloat('graph maxX', self.graphBBox.max.s + 0)
			or ig.igInputFloat('graph maxY', self.graphBBox.max.s + 1)
			or ig.igInputFloat('graph maxZ', self.graphBBox.max.s + 2)
			or ig.luatableInputFloat('mandelConst', self, 'mandelConst')
			or ig.luatableInputInt('mandelPerm', self, 'mandelPerm')
			or ig.luatableInputInt('mandelMaxIter', self, 'mandelMaxIter')
			or ig.luatableInputFloat('mandelEscapeNormSq', self, 'mandelEscapeNormSq')
			or ig.luatableCheckbox('recalcWithCompute', self, 'recalcWithCompute')
			then
				self:recalc()
			end

			ig.luatableCheckbox('autoUpdateValueRange', self, 'autoUpdateValueRange')

			if ig.igInputFloat('value min', self.vol.valueRange.s + 0)
			or ig.igInputFloat('value max', self.vol.valueRange.s + 1)
			then
				self.valueRangeDirty = true
				-- dirty means once we're on autoUpdateValueRange it will recalc
			end

			-- logDensity is normalized to this range [.x,.y], and then clamped to (0,1), and then scaled by .z
			ig.igInputFloat('densityAlphaPeriod', self.vol.densityAlphaRange.s + 0)
			ig.igSliderFloat('densityAlphaOffset', self.vol.densityAlphaRange.s + 1, 0, 1)
			ig.igInputFloat('densityAlphaGamma', self.vol.densityAlphaRange.s + 2)


			ig.luatableSliderFloat('alpha', self.vol, 'alpha', 0, 1)
			ig.luatableCheckbox('useLog', self.vol, 'useLog')

-- [[
			ig.igInputFloat('vol minX', self.worldBBox.min.s + 0)
			ig.igInputFloat('vol minY', self.worldBBox.min.s + 1)
			ig.igInputFloat('vol minZ', self.worldBBox.min.s + 2)

			ig.igInputFloat('vol maxX', self.worldBBox.max.s + 0)
			ig.igInputFloat('vol maxY', self.worldBBox.max.s + 1)
			ig.igInputFloat('vol maxZ', self.worldBBox.max.s + 2)
--]]
			ig.igEndMenu()
		end
		ig.igEndMainMenuBar()
	end
end

return App():run()
