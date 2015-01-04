{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}

module Haste.Graphics.WebGL.Framebuffer where

import Data.Bits
import Haste.DOM
import Haste.Foreign
import Haste.Prim
import System.IO.Unsafe (unsafePerformIO)

import Haste.Graphics.WebGL.Types
import Haste.Graphics.WebGL.Texture

newtype Framebuffer = Framebuffer JSAny deriving (Pack, Unpack)

data FramebufferStatus = FramebufferComplete | FramebufferUnsupported |
                         FramebufferIncompleteAttachment |
                         FramebufferIncompleteDimensions |
                         FramebufferIncompleteMissingAttachment

instance Enum FramebufferStatus where
  fromEnum FramebufferComplete = 0x8cd5
  fromEnum FramebufferUnsupported = 0x8cdd
  fromEnum FramebufferIncompleteAttachment = 0x8cd6
  fromEnum FramebufferIncompleteDimensions = 0x8cd9
  fromEnum FramebufferIncompleteMissingAttachment = 0x8cd7

  toEnum 0x8cd5 = FramebufferComplete
  toEnum 0x8cdd = FramebufferUnsupported
  toEnum 0x8cd6 = FramebufferIncompleteAttachment
  toEnum 0x8cd9 = FramebufferIncompleteDimensions
  toEnum 0x8cd7 = FramebufferIncompleteMissingAttachment
  toEnum _ = undefined

instance Pack FramebufferStatus where
  pack = toEnum . pack

instance Unpack FramebufferStatus where
  unpack = unpack . fromEnum

data FramebufferAttachment = ColorAttachment0 | DepthAttachment | StencilAttachment

instance Enum FramebufferAttachment where
  fromEnum ColorAttachment0 = 0x8ce0
  fromEnum DepthAttachment = 0x8d00
  fromEnum StencilAttachment = 0x8d20

  toEnum 0x8ce0 = ColorAttachment0
  toEnum 0x8d00 = DepthAttachment
  toEnum 0x8d20 = StencilAttachment
  toEnum _ = undefined

instance Pack FramebufferAttachment where
  pack = toEnum . pack

instance Unpack FramebufferAttachment where
  unpack = unpack . fromEnum

data FramebufferAttachmentPName = FramebufferAttachmentObjectType |
                                  FramebufferAttachmentObjectName |
                                  FramebufferAttachmentTextureLevel |
                                  FramebufferAttachmentTextureCubeMapFace

instance Enum FramebufferAttachmentPName where
  fromEnum FramebufferAttachmentObjectType = 0x8cd0
  fromEnum FramebufferAttachmentObjectName = 0x8cd1
  fromEnum FramebufferAttachmentTextureLevel = 0x8cd2
  fromEnum FramebufferAttachmentTextureCubeMapFace = 0x8cd3

  toEnum 0x8cd0 = FramebufferAttachmentObjectType
  toEnum 0x8cd1 = FramebufferAttachmentObjectName
  toEnum 0x8cd2 = FramebufferAttachmentTextureLevel
  toEnum 0x8cd3 = FramebufferAttachmentTextureCubeMapFace
  toEnum _ = undefined

instance Pack FramebufferAttachmentPName where
  pack = toEnum . pack

instance Unpack FramebufferAttachmentPName where
  unpack = unpack . fromEnum

noFramebuffer::Framebuffer
noFramebuffer = unsafePerformIO (ffi "(function() {return null;})"::IO Framebuffer)

bindFramebuffer::Context->Framebuffer->IO ()
bindFramebuffer = ffi "(function(ctx, framebuffer) {ctx.bindFramebuffer(ctx.FRAMEBUFFER, framebuffer);})"

checkFramebufferStatus::Context->IO FramebufferStatus
checkFramebufferStatus = ffi "(function(ctx) {return ctx.checkFramebufferStatus(ctx.FRAMEBUFFER);})"

createFramebuffer::Context->IO Framebuffer
createFramebuffer = ffi "(function(ctx) {return ctx.createFramebuffer();})"

deleteFramebuffer::Context->Framebuffer->IO ()
deleteFramebuffer = ffi "(function(ctx, buffer) {ctx.deleteFramebuffer(buffer);})"

framebufferRenderbuffer::Context->FramebufferAttachment->Framebuffer->IO ()
framebufferRenderbuffer = ffi "(function(ctx, attachment, renderbuffer) {ctx.framebufferRenderbuffer(ctx.FRAMEBUFFER, attachment, ctx.RENDERBUFFER, renderbuffer);})"

isFramebuffer::Context->Framebuffer->IO ()
isFramebuffer = ffi "(function(ctx, buffer) {return ctx.isFramebuffer(buffer);})"

framebufferTexture2D::Context->FramebufferAttachment->TextureTarget->Texture->Int->IO ()
framebufferTexture2D = ffi "(function(ctx, attachment, textarget, texture, level) {ctx.framebufferTexture2D(ctx.FRAMEBUFFER, attachment, textarget, texture, level);})"

getFramebufferAttachmentParameter::Context->FramebufferAttachment->FramebufferAttachmentPName->IO JSAny
getFramebufferAttachmentParameter = ffi "(function(ctx, attachment, pname) {return ctx.getFramebufferAttachmentParameter(ctx.FRAMEBUFFER, attachment, pname);})"
