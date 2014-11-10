{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}

module Haste.Graphics.WebGL.ViewClip where

import Haste.DOM
import Haste.Foreign
import Haste.Prim

import Haste.Graphics.WebGL.Types

depthRange::Context->Double->Double->IO ()
depthRange = ffi "(function(ctx, zNear, zFar) {ctx.depthRange(zNear, zFar);})"

scissor::Context->Int->Int->Int->Int->IO ()
scissor = ffi "(function(ctx, x, y, width, height) {ctx.scissor(x, y, width, height);})"

viewport::Context->Int->Int->Int->Int->IO ()
viewport = ffi "(function(ctx, x, y, width, height) {ctx.viewport(x, y, width, height);})"
