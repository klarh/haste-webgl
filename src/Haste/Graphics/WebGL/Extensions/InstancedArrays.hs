{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}

module Haste.Graphics.WebGL.Extensions.InstancedArrays where

import Haste.Foreign
import Haste.Prim
import Haste.Graphics.WebGL.UniformsAttributes (AttribLocation)

import Haste.Graphics.WebGL.Types

-- | An ANGLE_instanced_arrays-enabled webgl context object
newtype Extension = Extension JSAny deriving (Pack, Unpack)

getExtension::Context->IO Extension
getExtension = ffi "(function(ctx) {return ctx.getExtension(\"ANGLE_INSTANCED_ARRAYS\");})"

isExtensionValid::Extension->IO Bool
isExtensionValid = ffi "(function(ext) {return Boolean(ext);})"

vertexAttribDivisor::Extension->AttribLocation->Int->IO ()
vertexAttribDivisor = ffi "(function(ext, index, div) {ext.vertexAttribDivisorANGLE(index, div);})"

drawArraysInstanced::Extension->DrawMode->Int->Int->Int->IO ()
drawArraysInstanced = ffi "(function(ext, mode, first, count, primcount) {ext.drawArraysInstancedANGLE(mode, first, count, primcount);})"

drawElementsInstanced::Extension->DrawMode->Int->ElementType->Int->Int->IO ()
drawElementsInstanced = ffi "(function(ext, mode, count, offset, primcount) {ext.drawElementsInstancedANGLE(mode, count, ext.UNSIGNED_SHORT, offset, primcount);})"
