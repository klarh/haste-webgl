{-# LANGUAGE OverloadedStrings #-}

module Haste.Graphics.WebGL.FramebufferOperations where

import Data.Bits
import Haste.DOM
import Haste.Foreign
import Haste.Prim

import Haste.Graphics.WebGL.Types

-- TODO make sure we really want the interface to work like this,
-- using Data.Bits
data ClearBit = ColorBufferBit | DepthBufferBit | StencilBufferBit | ClearBitMask Int
              deriving Eq

instance Enum ClearBit where
  fromEnum ColorBufferBit = 0x4000
  fromEnum DepthBufferBit = 0x100
  fromEnum StencilBufferBit = 0x400
  fromEnum (ClearBitMask v) = v

  toEnum 0x4000 = ColorBufferBit
  toEnum 0x100 = DepthBufferBit
  toEnum 0x400 = StencilBufferBit
  toEnum v = ClearBitMask v

instance Pack ClearBit where
  pack = toEnum . pack

instance Unpack ClearBit where
  unpack = unpack . fromEnum

instance Bits ClearBit where
  x .|. y = ClearBitMask $ fromEnum x .|. fromEnum y
  (.&.) = undefined
  xor = undefined
  complement = undefined
  shift = undefined
  rotate = undefined
  bitSize = undefined
  isSigned = undefined
  testBit = undefined
  bit = undefined
  popCount = undefined

clear::Context->ClearBit->IO ()
clear = ffi "(function(ctx, mask) {ctx.clear(mask);})"

clearColor::Context->Double->Double->Double->Double->IO ()
clearColor = ffi "(function(ctx, r, g, b, a) {ctx.clearColor(r, g, b, a);})"

clearDepth::Context->Double->IO ()
clearDepth = ffi "(function(ctx, depth) {ctx.clearDepth(depth);})"

clearStencil::Context->Int->IO ()
clearStencil = ffi "(function(ctx, s) {ctx.clearStencil(s);})"

colorMask::Context->Bool->Bool->Bool->Bool->IO ()
colorMask = ffi "(function(ctx, r, g, b, a) {ctx.colorMask(r, g, b, a);})"

depthMask::Context->Bool->IO ()
depthMask = ffi "(function(ctx, flag) {ctx.depthMask(flag);})"

stencilMask::Context->Int->IO ()
stencilMask = ffi "(function(ctx, mask) {ctx.stencilMask(mask);})"

stencilMaskSeparate::Context->StencilFace->Int->IO ()
stencilMaskSeparate = ffi "(function(ctx, face, mask) {ctx.stencilMaskSeparate(face, mask);})"
