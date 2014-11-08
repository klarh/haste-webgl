{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}

module Haste.Graphics.WebGL where

import Haste.DOM
import Haste.Foreign
import Haste.Prim

import Haste.Graphics.WebGL.Types
import Haste.Graphics.WebGL.Arrays
import Haste.Graphics.WebGL.Buffer
import Haste.Graphics.WebGL.Framebuffer
import Haste.Graphics.WebGL.FramebufferOperations
import Haste.Graphics.WebGL.PerFragment
import Haste.Graphics.WebGL.ProgramsShaders
import Haste.Graphics.WebGL.Rasterization
import Haste.Graphics.WebGL.Special
import Haste.Graphics.WebGL.Texture
import Haste.Graphics.WebGL.UniformsAttributes
import Haste.Graphics.WebGL.ViewClip

getContext::Elem->String->IO Context
getContext = ffi "(function(elt, name) {return elt.getContext(name);})"

getSupportedExtensions::Context->IO [String]
getSupportedExtensions = ffi "(function(ctx) {return ctx.getSupportedExtensions();})"

getExtension::Context->String->IO JSAny
getExtension = ffi "(function(ctx, name) {return ctx.getExtension(name);})"

isContextLost::Context->IO Bool
isContextLost = ffi "(function(ctx) {return ctx.isContextLost();})"

drawArrays::Context->DrawMode->Int->Int->IO ()
drawArrays = ffi "(function(ctx, mode, first, count) {ctx.drawArrays(mode, first, count);})"

drawElements::Context->DrawMode->Int->ElementType->Int->IO ()
drawElements = ffi "(function(ctx, mode, count, type, offset) {ctx.drawElements(mode, count, type, offset);})"

readPixels::TypedArray a=>Context->Int->Int->Int->Int->a->IO ()
readPixels = ffi "(function(ctx, x, y, width, height, pixels) {ctx.readPixels(x, y, width, height, ctx.RGBA, ctx.UNSIGNED_BYTE, pixels);})"
