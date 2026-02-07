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
	self.mandelPerm = 0	-- 0-3


	-- range in 3D space
	self.volumeRange = box3f(
		{-1, -1, -1},
		{1, 1, 1}
	)

	self.cuboidWireframe = CuboidWireframe{
		view = self.view,
		color = {.5, .5, .5, 1},
	}
	-- set it to update every frame
	self.cuboidWireframe.globj.uniforms.mins = self.volumeRange.min.s
	self.cuboidWireframe.globj.uniforms.maxs = self.volumeRange.max.s


	self.volumeRenderer = VolumeRenderer{
		view = self.view,
		useLog = true,
		size = {64, 64, 64},
	}
	self.densityData = self.volumeRenderer.data
	self:recalc()
	-- set it to update every frame
	self.volumeRenderer.globj.uniforms.mins = self.volumeRange.min.s
	self.volumeRenderer.globj.uniforms.maxs = self.volumeRange.max.s

	gl.glEnable(gl.GL_DEPTH_TEST)
	gl.glDepthFunc(gl.GL_LEQUAL)	-- for the wireframe
end

function App:recalc()
	local b = box3f({-2, -2, -2}, {2, 2, 2})
	local ptr = self.densityData+0
	local fw = self.mandelConst
	local perm = self.mandelPerm
print('perm', perm)	-- I'm not seeing a difference	
	local densityRange = self.volumeRenderer.densityRange
	densityRange:set(math.huge, -math.huge)
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
						densityRange.x = math.min(densityRange.x, iter)
						densityRange.y = math.max(densityRange.y, iter)
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

	print('density range', densityRange)
end



function App:update()
	gl.glClear(bit.bor(gl.GL_COLOR_BUFFER_BIT, gl.GL_DEPTH_BUFFER_BIT))

	self.cuboidWireframe:draw()
	
	self.volumeRenderer:draw()

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

			ig.igInputFloat('densityMin', self.volumeRenderer.densityRange.s + 0)
			ig.igInputFloat('densityMax', self.volumeRenderer.densityRange.s + 1)

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

			ig.luatableCheckbox('useLog', self.volumeRenderer, 'useLog')
	
-- [[
			ig.igInputFloat('minX', self.volumeRange.min.s + 0)
			ig.igInputFloat('minY', self.volumeRange.min.s + 1)
			ig.igInputFloat('minZ', self.volumeRange.min.s + 2)

			ig.igInputFloat('maxX', self.volumeRange.max.s + 0)
			ig.igInputFloat('maxY', self.volumeRange.max.s + 1)
			ig.igInputFloat('maxZ', self.volumeRange.max.s + 2)
--]]
			ig.igEndMenu()
		end
		ig.igEndMainMenuBar()
	end
end

return App():run()
