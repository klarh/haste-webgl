{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}

module Haste.Graphics.WebGL.PerFragment where

import Haste.DOM
import Haste.Foreign
import Haste.Prim

import Haste.Graphics.WebGL.Types

data BlendMode = FuncAdd | FuncSubtract | FuncReverseSubtract

instance Enum BlendMode where
  fromEnum FuncAdd = 0x8006
  fromEnum FuncSubtract = 0x800a
  fromEnum FuncReverseSubtract = 0x800b

  toEnum 0x8006 = FuncAdd
  toEnum 0x800a = FuncSubtract
  toEnum 0x800b = FuncReverseSubtract
  toEnum _ = undefined

instance Pack BlendMode where
  pack = toEnum . pack

instance Unpack BlendMode where
  unpack = unpack . fromEnum

data SourceFactor = SFactorZero | SFactorOne | SFactorSrcColor |
                    SFactorOneMinusSrcColor | SFactorDstColor |
                    SFactorOneMinusDstColor | SFactorSrcAlpha |
                    SFactorOneMinusSrcAlpha | SFactorDstAlpha |
                    SFactorOneMinusDstAlpha | SFactorConstantColor |
                    SFactorOneMinusConstantColor | SFactorConstantAlpha |
                    SFactorOneMinusConstantAlpha | SFactorSrcAlphaSaturate

instance Enum SourceFactor where
  fromEnum SFactorZero = 0
  fromEnum SFactorOne = 1
  fromEnum SFactorSrcColor = 0x300
  fromEnum SFactorOneMinusSrcColor = 0x301
  fromEnum SFactorSrcAlpha = 0x302
  fromEnum SFactorOneMinusSrcAlpha = 0x303
  fromEnum SFactorDstAlpha = 0x304
  fromEnum SFactorOneMinusDstAlpha = 0x305
  fromEnum SFactorDstColor = 0x306
  fromEnum SFactorOneMinusDstColor = 0x307
  fromEnum SFactorSrcAlphaSaturate = 0x308
  fromEnum SFactorConstantColor = 0x8001
  fromEnum SFactorOneMinusConstantColor = 0x8002
  fromEnum SFactorConstantAlpha = 0x8003
  fromEnum SFactorOneMinusConstantAlpha = 0x8004

  toEnum 0 = SFactorZero
  toEnum 1 = SFactorOne
  toEnum 0x300 = SFactorSrcColor
  toEnum 0x301 = SFactorOneMinusSrcColor
  toEnum 0x302 = SFactorSrcAlpha
  toEnum 0x303 = SFactorOneMinusSrcAlpha
  toEnum 0x304 = SFactorDstAlpha
  toEnum 0x305 = SFactorOneMinusDstAlpha
  toEnum 0x306 = SFactorDstColor
  toEnum 0x307 = SFactorOneMinusDstColor
  toEnum 0x308 = SFactorSrcAlphaSaturate
  toEnum 0x8001 = SFactorConstantColor
  toEnum 0x8002 = SFactorOneMinusConstantColor
  toEnum 0x8003 = SFactorConstantAlpha
  toEnum 0x8004 = SFactorOneMinusConstantAlpha
  toEnum _ = undefined

instance Pack SourceFactor where
  pack = toEnum . pack

instance Unpack SourceFactor where
  unpack = unpack . fromEnum

data DestFactor = DFactorZero | DFactorOne | DFactorSrcColor |
                  DFactorOneMinusSrcColor | DFactorDstColor |
                  DFactorOneMinusDstColor | DFactorSrcAlpha |
                  DFactorOneMinusSrcAlpha | DFactorDstAlpha |
                  DFactorOneMinusDstAlpha | DFactorConstantColor |
                  DFactorOneMinusConstantColor | DFactorConstantAlpha |
                  DFactorOneMinusConstantAlpha

instance Enum DestFactor where
  fromEnum DFactorZero = 0
  fromEnum DFactorOne = 1
  fromEnum DFactorSrcColor = 0x300
  fromEnum DFactorOneMinusSrcColor = 0x301
  fromEnum DFactorSrcAlpha = 0x302
  fromEnum DFactorOneMinusSrcAlpha = 0x303
  fromEnum DFactorDstAlpha = 0x304
  fromEnum DFactorOneMinusDstAlpha = 0x305
  fromEnum DFactorDstColor = 0x306
  fromEnum DFactorOneMinusDstColor = 0x307
  fromEnum DFactorConstantColor = 0x8001
  fromEnum DFactorOneMinusConstantColor = 0x8002
  fromEnum DFactorConstantAlpha = 0x8003
  fromEnum DFactorOneMinusConstantAlpha = 0x8004

  toEnum 0 = DFactorZero
  toEnum 1 = DFactorOne
  toEnum 0x300 = DFactorSrcColor
  toEnum 0x301 = DFactorOneMinusSrcColor
  toEnum 0x302 = DFactorSrcAlpha
  toEnum 0x303 = DFactorOneMinusSrcAlpha
  toEnum 0x304 = DFactorDstAlpha
  toEnum 0x305 = DFactorOneMinusDstAlpha
  toEnum 0x306 = DFactorDstColor
  toEnum 0x307 = DFactorOneMinusDstColor
  toEnum 0x8001 = DFactorConstantColor
  toEnum 0x8002 = DFactorOneMinusConstantColor
  toEnum 0x8003 = DFactorConstantAlpha
  toEnum 0x8004 = DFactorOneMinusConstantAlpha
  toEnum _ = undefined

instance Pack DestFactor where
  pack = toEnum . pack

instance Unpack DestFactor where
  unpack = unpack . fromEnum

data DepthFunc = DepthNever | DepthAlways | DepthLess | DepthEqual | DepthLequal |
                 DepthGreater | DepthGequal | DepthNotEqual

instance Enum DepthFunc where
  fromEnum DepthNever = 0x200
  fromEnum DepthAlways = 0x207
  fromEnum DepthLess = 0x201
  fromEnum DepthEqual = 0x202
  fromEnum DepthLequal = 0x203
  fromEnum DepthGreater = 0x204
  fromEnum DepthGequal = 0x206
  fromEnum DepthNotEqual = 0x205

  toEnum 0x200 = DepthNever
  toEnum 0x207 = DepthAlways
  toEnum 0x201 = DepthLess
  toEnum 0x202 = DepthEqual
  toEnum 0x203 = DepthLequal
  toEnum 0x204 = DepthGreater
  toEnum 0x206 = DepthGequal
  toEnum 0x205 = DepthNotEqual
  toEnum _ = undefined

instance Pack DepthFunc where
  pack = toEnum . pack

instance Unpack DepthFunc where
  unpack = unpack . fromEnum

data StencilFunc = StencilNever | StencilAlways | StencilLess | StencilLequal |
                   StencilEqual | StencilNotEqual | StencilGreater | StencilGequal

instance Enum StencilFunc where
  fromEnum StencilNever = 0x200
  fromEnum StencilAlways = 0x207
  fromEnum StencilLess = 0x201
  fromEnum StencilLequal = 0x203
  fromEnum StencilEqual = 0x202
  fromEnum StencilNotEqual = 0x205
  fromEnum StencilGreater = 0x204
  fromEnum StencilGequal = 0x206

  toEnum 0x200 = StencilNever
  toEnum 0x207 = StencilAlways
  toEnum 0x201 = StencilLess
  toEnum 0x203 = StencilLequal
  toEnum 0x202 = StencilEqual
  toEnum 0x205 = StencilNotEqual
  toEnum 0x204 = StencilGreater
  toEnum 0x206 = StencilGequal
  toEnum _ = undefined

instance Pack StencilFunc where
  pack = toEnum . pack

instance Unpack StencilFunc where
  unpack = unpack . fromEnum

data StencilFail = FailKeep | FailZero | FailReplace | FailIncr | FailDecr |
                   FailInvert | FailIncrWrap | FailDecrWrap

instance Enum StencilFail where
  fromEnum FailKeep = 0x1e00
  fromEnum FailZero = 0x0
  fromEnum FailReplace = 0x1e01
  fromEnum FailIncr = 0x1e02
  fromEnum FailDecr = 0x1e03
  fromEnum FailInvert = 0x150a
  fromEnum FailIncrWrap = 0x8507
  fromEnum FailDecrWrap = 0x8508

  toEnum 0x1e00 = FailKeep
  toEnum 0x0 = FailZero
  toEnum 0x1e01 = FailReplace
  toEnum 0x1e02 = FailIncr
  toEnum 0x1e03 = FailDecr
  toEnum 0x150a = FailInvert
  toEnum 0x8507 = FailIncrWrap
  toEnum 0x8508 = FailDecrWrap
  toEnum _ = undefined

instance Pack StencilFail where
  pack = toEnum . pack

instance Unpack StencilFail where
  unpack = unpack . fromEnum

blendColor::Context->Double->Double->Double->Double->IO ()
blendColor = ffi "(function(ctx, red, green, blue, alpha) {ctx.blendColor(red, green, blue, alpha);})"

blendEquationSeparate::Context->BlendMode->BlendMode->IO ()
blendEquationSeparate = ffi "(function(ctx, modeRGB, modeAlpha) {ctx.blendEquationSeparate(modeRGB, modeAlpha);})"

blendFunc::Context->SourceFactor->DestFactor->IO ()
blendFunc = ffi "(function(ctx, sfactor, dfactor) {ctx.blendFunc(sfactor, dfactor);})"

blendFuncSeparate::Context->SourceFactor->DestFactor->SourceFactor->DestFactor->
                   IO ()
blendFuncSeparate = ffi "(function(ctx, srcRGB, dstRGB, srcAlpha, dstAlpha) {ctx.blendFuncSeparate(srcRGB, dstRGB, srcAlpha, dstAlpha);})"

depthFunc::Context->DepthFunc->IO ()
depthFunc = ffi "(function(ctx, func) {ctx.depthFunc(func);})"

sampleCoverage::Context->Double->Bool->IO ()
sampleCoverage = ffi "(function(ctx, value, invert) {ctx.sampleCoverage(value, invert);})"

stencilFunc::Context->StencilFunc->Int->Int->IO ()
stencilFunc = ffi "(function(ctx, func, ref, mask) {ctx.stencilFunc(func, ref, mask);})"

stencilFuncSeparate::Context->StencilFace->StencilFunc->Int->Int->IO ()
stencilFuncSeparate = ffi "(function(ctx, face, func, ref, mask) {ctx.stencilFuncSeparate(face, func, ref, mask);})"

stencilOp::Context->StencilFail->StencilFail->StencilFail->IO ()
stencilOp = ffi "(function(ctx, fail, zfail, zpass) {ctx.stencilOp(fail, zfail, zpass);})"

stencilOpSeparate::Context->StencilFace->StencilFail->StencilFail->StencilFail->IO ()
stencilOpSeparate = ffi "(function(ctx, face, fail, zfail, zpass) {ctx.stencilOpSeparate(face, fail, zfail, zpass);})"
