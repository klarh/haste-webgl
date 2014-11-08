{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}

module Haste.Graphics.WebGL.ProgramsShaders where

import Haste.DOM
import Haste.Foreign
import Haste.Prim

import Haste.Graphics.WebGL.Types

newtype Program = Program JSAny deriving (Pack, Unpack)

newtype Shader = Shader JSAny deriving (Pack, Unpack)

data ShaderType = VertexShader | FragmentShader

instance Enum ShaderType where
  fromEnum VertexShader = 0x8b31
  fromEnum FragmentShader = 0x8b30

  toEnum 0x8b31 = VertexShader
  toEnum 0x8b30 = FragmentShader
  toEnum _ = undefined

instance Pack ShaderType where
  pack = toEnum . pack

instance Unpack ShaderType where
  unpack = unpack . fromEnum

data ProgramPName = ProgramDeleteStatus | LinkStatus | ValidateStatus |
                    AttachedShaders | ActiveAttributes | ActiveUniforms

instance Enum ProgramPName where
  fromEnum ProgramDeleteStatus = 0x8b80
  fromEnum LinkStatus = 0x8b82
  fromEnum ValidateStatus = 0x8b83
  fromEnum AttachedShaders = 0x8b85
  fromEnum ActiveAttributes = 0x8b89
  fromEnum ActiveUniforms = 0x8b86

  toEnum 0x8b80 = ProgramDeleteStatus
  toEnum 0x8b82 = LinkStatus
  toEnum 0x8b83 = ValidateStatus
  toEnum 0x8b85 = AttachedShaders
  toEnum 0x8b89 = ActiveAttributes
  toEnum 0x8b86 = ActiveUniforms
  toEnum _ = undefined

instance Pack ProgramPName where
  pack = toEnum . pack

instance Unpack ProgramPName where
  unpack = unpack . fromEnum

data ShaderPName = ShaderTypePName | ShaderDeleteStatus | CompileStatus

instance Enum ShaderPName where
  fromEnum ShaderTypePName = 0x8b4f
  fromEnum ShaderDeleteStatus = 0x8b80
  fromEnum CompileStatus = 0x8b81

  toEnum 0x8b4f = ShaderTypePName
  toEnum 0x8b80 = ShaderDeleteStatus
  toEnum 0x8b81 = CompileStatus
  toEnum _ = undefined

instance Pack ShaderPName where
  pack = toEnum . pack

instance Unpack ShaderPName where
  unpack = unpack . fromEnum


attachShader::Context->Program->Shader->IO ()
attachShader = ffi "(function(ctx, program, shader) {ctx.attachShader(program, shader);})"

bindAttribLocation::Context->Program->Int->String->IO ()
bindAttribLocation = ffi "(function(ctx, program, index, name) {ctx.bindAttribLocation(program, index, name);})"

compileShader::Context->Shader->IO ()
compileShader = ffi "(function(ctx, shader) {ctx.compileShader(shader);})"

createProgram::Context->IO Program
createProgram = ffi "(function(ctx) {return ctx.createProgram();})"

createShader::Context->ShaderType->IO Shader
createShader = ffi "(function(ctx, type) {return ctx.createShader(type);})"

deleteProgram::Context->Program->IO ()
deleteProgram = ffi "(function(ctx, program) {ctx.deleteProgram(program);})"

deleteShader::Context->Shader->IO ()
deleteShader = ffi "(function(ctx, shader) {ctx.deleteShader(shader);})"

detachShader::Context->Program->Shader->IO ()
detachShader = ffi "(function(ctx, program, shader) {ctx.detachShader(program, shader);})"

getAttachedShaders::Context->Program->IO [Shader]
getAttachedShaders = ffi "(function(ctx, program) {return ctx.getAttachedShaders();})"

getProgramParameter::Context->Program->ProgramPName->IO JSAny
getProgramParameter = ffi "(function(ctx, program, pname) {return ctx.getProgramParameter(program, pname);})"

getProgramInfoLog::Context->Program->IO String
getProgramInfoLog = ffi "(function(ctx, program) {return ctx.getProgramInfoLog(program);})"

getShaderParameter::Context->Shader->ShaderPName->IO JSAny
getShaderParameter = ffi "(function(ctx, shader, pname) {return ctx.getShaderParameter(shader, pname);})"

getShaderInfoLog::Context->Shader->IO String
getShaderInfoLog = ffi "(function(ctx, shader) {return ctx.getShaderInfoLog(shader);})"

getShaderSource::Context->Shader->IO String
getShaderSource = ffi "(function(ctx, shader) {return ctx.getShaderSource();})"

-- TODO decide to let these take JSAny or Programs/Shaders
isProgram::Context->JSAny->IO Bool
isProgram = ffi "(function(ctx, ob) {return ctx.isProgram(ob);})"

isShader::Context->JSAny->IO Bool
isShader = ffi "(function(ctx, ob) {return ctx.isShader(ob);})"

linkProgram::Context->Program->IO ()
linkProgram = ffi "(function(ctx, program) {ctx.linkProgram(program);})"

shaderSource::Context->Shader->String->IO ()
shaderSource = ffi "(function(ctx, shader, source) {ctx.shaderSource(shader, source);})"

useProgram::Context->Program->IO ()
useProgram = ffi "(function(ctx, program) {ctx.useProgram(program);})"

validateProgram::Context->Program->IO ()
validateProgram = ffi "(function(ctx, program) {ctx.validateProgram(program);})"
