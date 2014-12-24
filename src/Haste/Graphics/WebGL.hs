{-# LANGUAGE OverloadedStrings #-}

module Haste.Graphics.WebGL (module Haste.Graphics.WebGL, module X)
       where

import Haste.DOM
import Haste.Foreign
import Haste.Prim

import Haste.JSArray.Typed as X
import Haste.Graphics.WebGL.Types as X
import Haste.Graphics.WebGL.Buffer as X
import Haste.Graphics.WebGL.Framebuffer as X
import Haste.Graphics.WebGL.FramebufferOperations as X
import Haste.Graphics.WebGL.PerFragment as X
import Haste.Graphics.WebGL.ProgramsShaders as X
import Haste.Graphics.WebGL.Rasterization as X
import Haste.Graphics.WebGL.Special as X
import Haste.Graphics.WebGL.Texture as X
import Haste.Graphics.WebGL.UniformsAttributes as X
import Haste.Graphics.WebGL.ViewClip as X

getContext::Elem->String->IO Context
getContext = ffi "(function(elt, name) {return elt.getContext(name);})"

isContextValid::Context->IO Bool
isContextValid = ffi "(function(ctx) {return Boolean(ctx);})"

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
