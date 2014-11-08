{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}

module Haste.Graphics.WebGL.Special where

import Haste.DOM
import Haste.Foreign
import Haste.Prim

import Haste.Graphics.WebGL.Types

data Capability = Blend | CullFace | DepthTest | Dither |
                  PolygonOffsetFill | SampleAlphaToCoverage | SampleCoverage |
                  ScissorTest | StencilTest

instance Enum Capability where
  fromEnum Blend = 0xbe2
  fromEnum CullFace = 0xb44
  fromEnum DepthTest = 0xb71
  fromEnum Dither = 0xbd0
  fromEnum PolygonOffsetFill = 0x8037
  fromEnum SampleAlphaToCoverage = 0x809e
  fromEnum SampleCoverage = 0x80a0
  fromEnum ScissorTest = 0xc11
  fromEnum StencilTest = 0xb90

  toEnum 0xbe2 = Blend
  toEnum 0xb44 = CullFace
  toEnum 0xb71 = DepthTest
  toEnum 0xbd0 = Dither
  toEnum 0x8037 = PolygonOffsetFill
  toEnum 0x809e = SampleAlphaToCoverage
  toEnum 0x80a0 = SampleCoverage
  toEnum 0xc11 = ScissorTest
  toEnum 0xb90 = StencilTest
  toEnum _ = undefined

instance Pack Capability where
  pack = toEnum . pack

instance Unpack Capability where
  unpack = unpack . fromEnum

data GLError = OutOfMemory | InvalidEnum | InvalidOperation |
               InvalidFramebufferOperation | InvalidValue |
               NoError | ContextLostWebGL

instance Enum GLError where
  fromEnum OutOfMemory = 0x505
  fromEnum InvalidEnum = 0x500
  fromEnum InvalidOperation = 0x502
  fromEnum InvalidFramebufferOperation = 0x506
  fromEnum InvalidValue = 0x501
  fromEnum NoError = 0x0
  fromEnum ContextLostWebGL = 0x9242

  toEnum 0x505 = OutOfMemory
  toEnum 0x500 = InvalidEnum
  toEnum 0x502 = InvalidOperation
  toEnum 0x506 = InvalidFramebufferOperation
  toEnum 0x501 = InvalidValue
  toEnum 0x0 = NoError
  toEnum 0x9242 = ContextLostWebGL
  toEnum _ = undefined

instance Pack GLError where
  pack = toEnum . pack

instance Unpack GLError where
  unpack = unpack . fromEnum

data GLParameter = AlphaBits | RedBits | GreenBits | BlueBits | SubpixelBits |
                   ActiveTexture | AliasedLineWidthRange | AliasedPointSizeRange |
                   ArrayBufferBinding | BlendDstAlpha | BlendDstRGB |
                   BlendEquationAlpha | BlendEquationRGB | BlendSrcAlpha |
                   BlendSrcRGB | BlendParam | BlendColor | ColorClearValue |
                   ColorWritemask | CompressedTextureFormats |
                   CullFaceParam | CullFaceMode |
                   CurrentProgram | DepthBits | DepthClearValue | DepthFunc |
                   DepthRange | DepthTestParam | DepthWritemask |
                   ElementArrayBufferBinding | DitherParam | FramebufferBinding |
                   FrontFace | GenerateMipmapHint | LineWidth |
                   MaxCombinedTextureImageUnits | MaxTextureImageUnits |
                   MaxCubeMapTextureSize | MaxRenderbufferSize |
                   MaxTextureSize | MaxVaryingVectors | MaxVertexAttribs |
                   MaxVertexTextureImageUnits | MaxVertexUniformVectors |
                   MaxViewportDims | PackAlignmentParam | PolygonOffsetFactor |
                   PolygonOffsetFillParam | PolygonOffsetUnits |
                   RenderbufferBinding | Renderer | SampleBuffers |
                   SampleCoverageInvert | SampleCoverageValue | Samples |
                   ScissorBox | ScissorTestParam | ShadingLanguageVersion |
                   StencilBits | StencilClearValue | StencilTestParam |
                   StencilFail | StencilFunc | StencilRef | StencilValueMask |
                   StencilWritemask | StencilBackFail | StencilBackFunc |
                   StencilBackRef | StencilBackValueMask | StencilBackWritemask |
                   StencilPassDepthFail | StencilPassDepthPass |
                   StencilBackPassDepthFail | StencilBackPassDepthPass |
                   TextureBinding2D | TextureBindingCubeMap |
                   UnpackAlignmentParam | UnpackColorspaceConversionWebGLParam |
                   UnpackFlipYWebGLParam | UnpackPremultiplyAlphaWebGLParam | Vendor |
                   Version | Viewport

instance Enum GLParameter where
  fromEnum AlphaBits = 0xd55
  fromEnum RedBits = 0xd52
  fromEnum GreenBits = 0xd53
  fromEnum BlueBits = 0xd54
  fromEnum SubpixelBits = 0xd50
  fromEnum ActiveTexture = 0x84e0
  fromEnum AliasedLineWidthRange = 0x846e
  fromEnum AliasedPointSizeRange = 0x846d
  fromEnum ArrayBufferBinding = 0x8894
  fromEnum BlendDstAlpha = 0x80ca
  fromEnum BlendDstRGB = 0x80c8
  fromEnum BlendEquationAlpha = 0x883d
  fromEnum BlendEquationRGB = 0x8009
  fromEnum BlendSrcAlpha = 0x80cb
  fromEnum BlendSrcRGB = 0x80c9
  fromEnum BlendParam = 0x8009
  fromEnum BlendColor = 0x8005
  fromEnum ColorClearValue = 0xc22
  fromEnum ColorWritemask = 0xc23
  fromEnum CompressedTextureFormats = 0x86a3
  fromEnum CullFaceParam = 0xb44
  fromEnum CullFaceMode = 0xb45
  fromEnum CurrentProgram = 0x8b8d
  fromEnum DepthBits = 0xd56
  fromEnum DepthClearValue = 0xb73
  fromEnum DepthFunc = 0xb74
  fromEnum DepthRange = 0xb70
  fromEnum DepthTestParam = 0xb71
  fromEnum DepthWritemask = 0xb72
  fromEnum ElementArrayBufferBinding = 0x8895
  fromEnum DitherParam = 0xbd0
  fromEnum FramebufferBinding = 0x8ca6
  fromEnum FrontFace = 0x0b46
  fromEnum GenerateMipmapHint = 0x8192
  fromEnum LineWidth = 0x0b21
  fromEnum MaxCombinedTextureImageUnits = 0x8b4d
  fromEnum MaxTextureImageUnits = 0x8872
  fromEnum MaxCubeMapTextureSize = 0x851c
  fromEnum MaxRenderbufferSize = 0x84e8
  fromEnum MaxTextureSize = 0x0d33
  fromEnum MaxVaryingVectors = 0x8dfc
  fromEnum MaxVertexAttribs = 0x8869
  fromEnum MaxVertexTextureImageUnits = 0x8b4c
  fromEnum MaxVertexUniformVectors = 0x8dfb
  fromEnum MaxViewportDims = 0xd3a
  fromEnum PackAlignmentParam = 0xd05
  fromEnum PolygonOffsetFactor = 0x8038
  fromEnum PolygonOffsetFillParam = 0x8037
  fromEnum PolygonOffsetUnits = 0x2a00
  fromEnum RenderbufferBinding = 0x8ca7
  fromEnum Renderer = 0x1f01
  fromEnum SampleBuffers = 0x80a8
  fromEnum SampleCoverageInvert = 0x80ab
  fromEnum SampleCoverageValue = 0x80aa
  fromEnum Samples = 0x80a9
  fromEnum ScissorBox = 0xc10
  fromEnum ScissorTestParam = 0xc11
  fromEnum ShadingLanguageVersion = 0x8b8c
  fromEnum StencilBits = 0x0d57
  fromEnum StencilClearValue = 0xb91
  fromEnum StencilTestParam = 0xb90
  fromEnum StencilFail = 0xb94
  fromEnum StencilFunc = 0xb92
  fromEnum StencilRef = 0xb97
  fromEnum StencilValueMask = 0xb93
  fromEnum StencilWritemask = 0xb98
  fromEnum StencilBackFail = 0x8801
  fromEnum StencilBackFunc = 0x8800
  fromEnum StencilBackRef = 0x8ca3
  fromEnum StencilBackValueMask = 0x8ca4
  fromEnum StencilBackWritemask = 0x8ca5
  fromEnum StencilPassDepthFail = 0xb95
  fromEnum StencilPassDepthPass = 0xb96
  fromEnum StencilBackPassDepthFail = 0x8802
  fromEnum StencilBackPassDepthPass = 0x8803
  fromEnum TextureBinding2D = 0x8069
  fromEnum TextureBindingCubeMap = 0x8514
  fromEnum UnpackAlignmentParam = 0xcf5
  fromEnum UnpackColorspaceConversionWebGLParam = 0x9243
  fromEnum UnpackFlipYWebGLParam = 0x9240
  fromEnum UnpackPremultiplyAlphaWebGLParam = 0x9241
  fromEnum Vendor = 0x1f00
  fromEnum Version = 0x1f02
  fromEnum Viewport = 0xba2

  toEnum 0xd55 = AlphaBits
  toEnum 0xd52 = RedBits
  toEnum 0xd53 = GreenBits
  toEnum 0xd54 = BlueBits
  toEnum 0xd50 = SubpixelBits
  toEnum 0x84e0 = ActiveTexture
  toEnum 0x846e = AliasedLineWidthRange
  toEnum 0x846d = AliasedPointSizeRange
  toEnum 0x8894 = ArrayBufferBinding
  toEnum 0x80ca = BlendDstAlpha
  toEnum 0x80c8 = BlendDstRGB
  toEnum 0x883d = BlendEquationAlpha
  toEnum 0x8009 = BlendEquationRGB
  toEnum 0x80cb = BlendSrcAlpha
  toEnum 0x80c9 = BlendSrcRGB
  toEnum 0x8009 = BlendParam
  toEnum 0x8005 = BlendColor
  toEnum 0xc22 = ColorClearValue
  toEnum 0xc23 = ColorWritemask
  toEnum 0x86a3 = CompressedTextureFormats
  toEnum 0xb44 = CullFaceParam
  toEnum 0xb45 = CullFaceMode
  toEnum 0x8b8d = CurrentProgram
  toEnum 0xd56 = DepthBits
  toEnum 0xb73 = DepthClearValue
  toEnum 0xb74 = DepthFunc
  toEnum 0xb70 = DepthRange
  toEnum 0xb71 = DepthTestParam
  toEnum 0xb72 = DepthWritemask
  toEnum 0x8895 = ElementArrayBufferBinding
  toEnum 0xbd0 = DitherParam
  toEnum 0x8ca6 = FramebufferBinding
  toEnum 0x0b46 = FrontFace
  toEnum 0x8192 = GenerateMipmapHint
  toEnum 0x0b21 = LineWidth
  toEnum 0x8b4d = MaxCombinedTextureImageUnits
  toEnum 0x8872 = MaxTextureImageUnits
  toEnum 0x851c = MaxCubeMapTextureSize
  toEnum 0x84e8 = MaxRenderbufferSize
  toEnum 0x0d33 = MaxTextureSize
  toEnum 0x8dfc = MaxVaryingVectors
  toEnum 0x8869 = MaxVertexAttribs
  toEnum 0x8b4c = MaxVertexTextureImageUnits
  toEnum 0x8dfb = MaxVertexUniformVectors
  toEnum 0xd3a = MaxViewportDims
  toEnum 0xd05 = PackAlignmentParam
  toEnum 0x8038 = PolygonOffsetFactor
  toEnum 0x8037 = PolygonOffsetFillParam
  toEnum 0x2a00 = PolygonOffsetUnits
  toEnum 0x8ca7 = RenderbufferBinding
  toEnum 0x1f01 = Renderer
  toEnum 0x80a8 = SampleBuffers
  toEnum 0x80ab = SampleCoverageInvert
  toEnum 0x80aa = SampleCoverageValue
  toEnum 0x80a9 = Samples
  toEnum 0xc10 = ScissorBox
  toEnum 0xc11 = ScissorTestParam
  toEnum 0x8b8c = ShadingLanguageVersion
  toEnum 0x0d57 = StencilBits
  toEnum 0xb91 = StencilClearValue
  toEnum 0xb90 = StencilTestParam
  toEnum 0xb94 = StencilFail
  toEnum 0xb92 = StencilFunc
  toEnum 0xb97 = StencilRef
  toEnum 0xb93 = StencilValueMask
  toEnum 0xb98 = StencilWritemask
  toEnum 0x8801 = StencilBackFail
  toEnum 0x8800 = StencilBackFunc
  toEnum 0x8ca3 = StencilBackRef
  toEnum 0x8ca4 = StencilBackValueMask
  toEnum 0x8ca5 = StencilBackWritemask
  toEnum 0xb95 = StencilPassDepthFail
  toEnum 0xb96 = StencilPassDepthPass
  toEnum 0x8802 = StencilBackPassDepthFail
  toEnum 0x8803 = StencilBackPassDepthPass
  toEnum 0x8069 = TextureBinding2D
  toEnum 0x8514 = TextureBindingCubeMap
  toEnum 0xcf5 = UnpackAlignmentParam
  toEnum 0x9243 = UnpackColorspaceConversionWebGLParam
  toEnum 0x9240 = UnpackFlipYWebGLParam
  toEnum 0x9241 = UnpackPremultiplyAlphaWebGLParam
  toEnum 0x1f00 = Vendor
  toEnum 0x1f02 = Version
  toEnum 0xba2 = Viewport
  toEnum _ = undefined

instance Pack GLParameter where
  pack = toEnum . pack

instance Unpack GLParameter where
  unpack = unpack . fromEnum

data HintMode = Fastest | Nicest | DontCare

instance Enum HintMode where
  fromEnum Fastest = 0x1101
  fromEnum Nicest = 0x1102
  fromEnum DontCare = 0x1100

  toEnum 0x1101 = Fastest
  toEnum 0x1102 = Nicest
  toEnum 0x1100 = DontCare
  toEnum _ = undefined

instance Pack HintMode where
  pack = toEnum . pack

instance Unpack HintMode where
  unpack = unpack . fromEnum

data PixelPName = UnpackAlignment | PackAlignment | UnpackFlipYWebGL |
                  UnpackPremultiplyAlphaWebGL | UnpackColorspaceConversionWebGL

instance Enum PixelPName where
  fromEnum UnpackAlignment = 0xcf5
  fromEnum PackAlignment = 0xd05
  fromEnum UnpackFlipYWebGL = 0x9240
  fromEnum UnpackPremultiplyAlphaWebGL = 0x9241
  fromEnum UnpackColorspaceConversionWebGL = 0x9243

  toEnum 0xcf5 = UnpackAlignment
  toEnum 0xd05 = PackAlignment
  toEnum 0x9240 = UnpackFlipYWebGL
  toEnum 0x9241 = UnpackPremultiplyAlphaWebGL
  toEnum 0x9243 = UnpackColorspaceConversionWebGL
  toEnum _ = undefined

instance Pack PixelPName where
  pack = toEnum . pack

instance Unpack PixelPName where
  unpack = unpack . fromEnum

disable::Context->Capability->IO ()
disable = ffi "(function(ctx, cap) {ctx.disable(cap);})"

enable::Context->Capability->IO ()
enable = ffi "(function(ctx, cap) {ctx.enable(cap);})"

finish::Context->IO ()
finish = ffi "(function(ctx) {ctx.finish();})"

flush::Context->IO ()
flush = ffi "(function(ctx) {ctx.flush();})"

getError::Context->IO GLError
getError = ffi "(function(ctx) {return ctx.getError();})"

getParameter::Context->GLParameter->IO JSAny
getParameter = ffi "(function(ctx, pname) {return ctx.getParameter(pname);})"

hint::Context->HintMode->IO ()
hint = ffi "(function(ctx, mode) {ctx.hint(ctx.GENERATE_MIPMAP_HINT, mode);})"

isEnabled::Context->Capability->IO Bool
isEnabled = ffi "(function(ctx, cap) {return ctx.isEnabled(cap);})"

pixelStorei::Context->PixelPName->Int->IO ()
pixelStorei = ffi "(function(ctx, pname, val) {ctx.pixelStorei(pname, val);})"
