#!/usr/bin/env luajit
local ffi = require 'ffi'
local range = require 'ext.range'
local cmdline = require 'ext.cmdline'(...)
local vec3i = require 'vec-ffi.vec3i'
local box3f = require 'vec-ffi.box3f'
local gl = require 'gl.setup'(cmdline.gl)
local ig = require 'imgui'
local CuboidWireframe = require 'volume-renderer.cuboid-wireframe'
local VolumeRenderer = require 'volume-renderer'


local App = require 'imgui.appwithorbit'()

function App:initGL()
	App.super.initGL(self)

	self.mandelConst = -.5
	self.mandelPerm = 2	-- 0-3


	-- range in 3D space
	self.bbox = box3f(
		{-1, -1, -1},
		{1, 1, 1}
	)

	self.cuboidWireframe = CuboidWireframe{
		view = self.view,
		color = {.5, .5, .5, 1},
	}
	-- set it to update every frame
	self.cuboidWireframe.globj.uniforms.mins = self.bbox.min.s
	self.cuboidWireframe.globj.uniforms.maxs = self.bbox.max.s


	self.volumeRenderer = VolumeRenderer{
		view = self.view,
		size = {128, 128, 128},
		--useLog = true,
	}
	-- set it to update every frame
	self.volumeRenderer.globj.uniforms.mins = self.bbox.min.s
	self.volumeRenderer.globj.uniforms.maxs = self.bbox.max.s
	
	self.graphSize = box3f({-2, -2, -2}, {2, 2, 2})
	self:recalc()
	
	gl.glEnable(gl.GL_DEPTH_TEST)
end

function App:recalc()
	local b = self.graphSize
	local ptr = self.volumeRenderer.data+0
	local fw = self.mandelConst
	local perm = self.mandelPerm
--print('perm', perm)	-- I'm not seeing a difference	
	local valueRange = self.volumeRenderer.valueRange
	valueRange:set(math.huge, -math.huge)
	local size = self.volumeRenderer.size
	for k=0,size.z-1 do
		local fz = (k+.5)/tonumber(size.z) * (b.max.z - b.min.z) + b.min.z
		for j=0,size.y-1 do
			local fy = (j+.5)/tonumber(size.y) * (b.max.y - b.min.y) + b.min.y
			for i=0,size.x-1 do
				local fx = (i+.5)/tonumber(size.x) * (b.max.x - b.min.x) + b.min.x
				
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
						valueRange.x = math.min(valueRange.x, iter)
						valueRange.y = math.max(valueRange.y, iter)
						break
					end
				end

				ptr=ptr+1
			end
		end
	end

	self.volumeRenderer.tex
		:bind()
		:subimage()
		:unbind()
--	print('density range', valueRange)
end

function App:update()
	gl.glClear(bit.bor(gl.GL_COLOR_BUFFER_BIT, gl.GL_DEPTH_BUFFER_BIT))

	self.cuboidWireframe:draw()
	
	self.volumeRenderer:draw()


	-- do mouse ray -> intersect with self.bbox
	-- click to recenter on axis of side of click
	-- click+drag to change bounds (TODO maintain aspect ratio?)
	--print(self.mouse.pos)


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

			if ig.igInputFloat('graph minX', self.graphSize.min.s + 0)
			or ig.igInputFloat('graph minY', self.graphSize.min.s + 1)
			or ig.igInputFloat('graph minZ', self.graphSize.min.s + 2)
			or ig.igInputFloat('graph maxX', self.graphSize.max.s + 0)
			or ig.igInputFloat('graph maxY', self.graphSize.max.s + 1)
			or ig.igInputFloat('graph maxZ', self.graphSize.max.s + 2)
			then
				self:recalc()
			end

			ig.igInputFloat('value min', self.volumeRenderer.valueRange.s + 0)
			ig.igInputFloat('value max', self.volumeRenderer.valueRange.s + 1)

			-- logDensity is normalized to this range [.x,.y], and then clamped to (0,1), and then scaled by .z
			ig.igInputFloat('densityAlphaPeriod', self.volumeRenderer.densityAlphaRange.s + 0)
			ig.igSliderFloat('densityAlphaOffset', self.volumeRenderer.densityAlphaRange.s + 1, 0, 1)
			ig.igInputFloat('densityAlphaGamma', self.volumeRenderer.densityAlphaRange.s + 2)

			if ig.luatableInputFloat('mandelConst', self, 'mandelConst') then
				self:recalc()
			end
			if ig.luatableInputInt('mandelPerm', self, 'mandelPerm') then
				self:recalc()
			end

			ig.luatableSliderFloat('alpha', self.volumeRenderer, 'alpha', 0, 1)
			ig.luatableCheckbox('useLog', self.volumeRenderer, 'useLog')
	
-- [[
			ig.igInputFloat('vol minX', self.bbox.min.s + 0)
			ig.igInputFloat('vol minY', self.bbox.min.s + 1)
			ig.igInputFloat('vol minZ', self.bbox.min.s + 2)

			ig.igInputFloat('vol maxX', self.bbox.max.s + 0)
			ig.igInputFloat('vol maxY', self.bbox.max.s + 1)
			ig.igInputFloat('vol maxZ', self.bbox.max.s + 2)
--]]
			ig.igEndMenu()
		end
		ig.igEndMainMenuBar()
	end
end

return App():run()
