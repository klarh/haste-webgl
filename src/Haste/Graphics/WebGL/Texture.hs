{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}

module Haste.Graphics.WebGL.Texture where

import Haste.DOM
import Haste.Foreign
import Haste.Prim

import Haste.Graphics.WebGL.Types

newtype Texture = Uniform JSAny deriving (Pack, Unpack)

data TextureEnum = TextureEnum Int

instance Enum TextureEnum where
  fromEnum (TextureEnum ix) = 0x84c0 + ix
  toEnum v
    | v >= 0x84c0 && v < 0x84e0 = TextureEnum (v - 0x84c0)
    | otherwise = undefined

instance Pack TextureEnum where
  pack = toEnum . pack

instance Unpack TextureEnum where
  unpack = unpack . fromEnum

data TextureType = Texture2D | TextureCubeMap

instance Enum TextureType where
  fromEnum Texture2D = 0xde1
  fromEnum TextureCubeMap = 0x8513

  toEnum 0xde1 = Texture2D
  toEnum 0x8513 = TextureCubeMap
  toEnum _ = undefined

instance Pack TextureType where
  pack = toEnum . pack

instance Unpack TextureType where
  unpack = unpack . fromEnum

data TextureTarget = Texture2DTarget | TextureCubeMapPositiveX |
                     TextureCubeMapPositiveY | TextureCubeMapPositiveZ |
                     TextureCubeMapNegativeX | TextureCubeMapNegativeY |
                     TextureCubeMapNegativeZ

instance Enum TextureTarget where
  fromEnum Texture2DTarget = 0xde1
  fromEnum TextureCubeMapPositiveX = 0x8515
  fromEnum TextureCubeMapPositiveY = 0x8517
  fromEnum TextureCubeMapPositiveZ = 0x8519
  fromEnum TextureCubeMapNegativeX = 0x8516
  fromEnum TextureCubeMapNegativeY = 0x8518
  fromEnum TextureCubeMapNegativeZ = 0x851a

  toEnum 0xde1 = Texture2DTarget
  toEnum 0x8515 = TextureCubeMapPositiveX
  toEnum 0x8517 = TextureCubeMapPositiveY
  toEnum 0x8519 = TextureCubeMapPositiveZ
  toEnum 0x8516 = TextureCubeMapNegativeX
  toEnum 0x8518 = TextureCubeMapNegativeY
  toEnum 0x851a = TextureCubeMapNegativeZ
  toEnum _ = undefined

instance Pack TextureTarget where
  pack = toEnum . pack

instance Unpack TextureTarget where
  unpack = unpack . fromEnum

data TextureFormat = Alpha | Luminance | LuminanceAlpha | RGB | RGBA

instance Enum TextureFormat where
  fromEnum Alpha = 0x1906
  fromEnum Luminance = 0x1909
  fromEnum LuminanceAlpha = 0x190a
  fromEnum RGB = 0x1907
  fromEnum RGBA = 0x1908

  toEnum 0x1906 = Alpha
  toEnum 0x1909 = Luminance
  toEnum 0x190a = LuminanceAlpha
  toEnum 0x1907 = RGB
  toEnum 0x1908 = RGBA
  toEnum _ = undefined

instance Pack TextureFormat where
  pack = toEnum . pack

instance Unpack TextureFormat where
  unpack = unpack . fromEnum

data TextureEltType = TexEltUnsignedByte | UnsignedShort565 |
                      UnsignedShort4444 | UnsignedShort5551

instance Enum TextureEltType where
  fromEnum TexEltUnsignedByte = 0x1401
  fromEnum UnsignedShort565 = 0x8363
  fromEnum UnsignedShort4444 = 0x8033
  fromEnum UnsignedShort5551 = 0x8034

  toEnum 0x1401 = TexEltUnsignedByte
  toEnum 0x8363 = UnsignedShort565
  toEnum 0x8033 = UnsignedShort4444
  toEnum 0x8034 = UnsignedShort5551
  toEnum _ = undefined

instance Pack TextureEltType where
  pack = toEnum . pack

instance Unpack TextureEltType where
  unpack = unpack . fromEnum

data TexPName = TextureWrapS | TextureWrapT |
                TextureMinFilter | TextureMagFilter

instance Enum TexPName where
  fromEnum TextureWrapS = 0x2802
  fromEnum TextureWrapT = 0x2803
  fromEnum TextureMinFilter = 0x2801
  fromEnum TextureMagFilter = 0x2800

  toEnum 0x2802 = TextureWrapS
  toEnum 0x2803 = TextureWrapT
  toEnum 0x2801 = TextureMinFilter
  toEnum 0x2800 = TextureMagFilter
  toEnum _ = undefined

instance Pack TexPName where
  pack = toEnum . pack

instance Unpack TexPName where
  unpack = unpack . fromEnum

activeTexture::Context->TextureEnum->IO ()
activeTexture = ffi "(function(ctx, texture) {ctx.activeTexture(texture);})"

bindTexture::Context->TextureType->Texture->IO ()
bindTexture = ffi "(function(ctx, target, texture) {ctx.bindTexture(target, texture);})"

copyTexImage2D::Context->TextureTarget->Int->TextureFormat->Int->Int->Int->Int->Int->IO ()
copyTexImage2D = ffi "(function(ctx, target, level, internalformat, x, y, width, height, border) {ctx.copyTexImage2D(target, level, internalformat, x, y, width, height, border);})"

copyTexSubImage2D::Context->TextureTarget->Int->Int->Int->Int->Int->Int->Int->IO ()
copyTexSubImage2D = ffi "(function(ctx, target, level, xoffset, yoffset, x, y, width, height) {ctx.copyTexSubImage2D(target, legel, xoffset, yoffset, x, y, width, height);})"

createTexture::Context->IO Texture
createTexture = ffi "(function(ctx) {return ctx.createTexture();})"

deleteTexture::Context->Texture->IO ()
deleteTexture = ffi "(function(ctx, texture) {ctx.deleteTexture(texture);})"

generateMipmap::Context->TextureType->IO ()
generateMipmap = ffi "(function(ctx, target) {ctx.generateMipmap(target);})"

getTexParameter::Context->TextureType->TexPName->IO Int
getTexParameter = ffi "(function(ctx, target, pname) {return ctx.getTexParameter(target, pname);})"

isTexture::Context->Texture->IO Bool
isTexture = ffi "(function(ctx, texture) {return ctx.isTexture(texture);})"

texImage2D::Context->TextureTarget->Int->TextureFormat->TextureFormat->TextureEltType->JSAny->IO ()
texImage2D = ffi "(function(ctx, target, level, internalformat, format, type, object) {ctx.texImage2D(target, level, internalformat, format, type, object);})"

texParameterf::Context->TextureType->TexPName->Double->IO ()
texParameterf = ffi "(function(ctx, target, pname, param) {ctx.texParameterf(target, pname, param);})"

texParameteri::Context->TextureType->TexPName->Int->IO ()
texParameteri = ffi "(function(ctx, target, pname, param) {ctx.texParameteri(target, pname, param);})"

texSubImage2DSized::Context->TextureTarget->Int->Int->Int->Int->Int->TextureFormat->TextureEltType->JSAny->IO ()
texSubImage2DSized = ffi "(function(ctx, target, level, xoffset, yoffset, width, height, format, type, pixels) {ctx.texSubImage2D(target, level, xoffset, yoffset, width, height, format, type, pixels);})"

texSubImage2D::Context->TextureTarget->Int->Int->Int->TextureFormat->TextureEltType->JSAny->IO ()
texSubImage2D = ffi "(function(ctx, target, level, xoffset, yoffset, format, type, object) {ctx.texSubImage2D(target, level, xoffset, yoffset, format, type, object);})"
