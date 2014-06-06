{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}

module Haste.Graphics.WebGL where

import Control.Applicative
import Data.Foldable
import Data.Vec
import Haste.DOM
import Haste.Foreign
import Haste.Prim
import Foreign.Ptr
import System.IO.Unsafe
import Haste.TypedArray as TA

newtype Context = Context JSAny deriving (Pack, Unpack)

data DrawMode = Points | LineStrip | LineLoop | Lines | TriangleStrip |
                TriangleFan | Triangles
newtype DrawMode' = DrawMode' JSAny deriving (Pack, Unpack)

newtype Shader = Shader JSAny deriving (Pack, Unpack)

data ShaderType = VertexShader | FragmentShader
newtype ShaderType' = ShaderType' JSAny deriving (Pack, Unpack)

newtype Program = Program JSAny deriving (Pack, Unpack)

newtype AttribLocation = AttribLocation JSAny deriving (Pack, Unpack)

newtype UniformLocation = UniformLocation JSAny deriving (Pack, Unpack)

newtype Buffer = Buffer JSAny deriving (Pack, Unpack)

data BufferUsage = StaticDraw | StreamDraw | DynamicDraw
newtype BufferUsage' = BufferUsage' JSAny deriving (Pack, Unpack)

data BufferType = ArrayBuffer | ElementArrayBuffer
newtype BufferType' = BufferType' JSAny deriving (Pack, Unpack)

data AttribType = Byte | Short | UnsignedByte | UnsignedShort | Fixed | Float
newtype AttribType' = AttribType' JSAny deriving (Pack, Unpack)

data ClearBit = ColorBufferBit | DepthBufferBit | StencilBufferBit
newtype ClearBit' = ClearBit' JSAny deriving (Pack, Unpack)

data Ability = Blend | CullFace | DepthTest | Dither | PolygonOffsetFill |
               SampleAlphaToCoverage | SampleCoverage | ScissorTest | StencilTest
newtype Ability' = Ability' JSAny deriving (Pack, Unpack)

getContext::Elem->String->IO Context
getContext = ffi "(function(elt, name) {return elt.getContext(name);})"

createShader::Context->ShaderType->IO Shader
createShader ctx typ = createShader' ctx typ'
  where
    createShader'::Context->ShaderType'->IO Shader
    createShader' = ffi "(function(ctx, typ) {return ctx.createShader(typ);})"

    idxShaderType::Context->ShaderType->ShaderType'
    idxShaderType ctx typ = unsafePerformIO $ idxShaderType' ctx strtyp
      where
        idxShaderType'::Context->String->IO ShaderType'
        idxShaderType' = ffi "(function(ctx, typ) {return ctx[typ];})"
        strtyp = case typ of
          VertexShader -> "VERTEX_SHADER"
          _ -> "FRAGMENT_SHADER"

    typ' = idxShaderType ctx typ

shaderSource::Context->Shader->String->IO ()
shaderSource = ffi "(function(ctx, shader, contents) {ctx.shaderSource(shader, contents);})"

compileShader::Context->Shader->IO ()
compileShader = ffi "(function(ctx, shader) {ctx.compileShader(shader);})"

attachShader::Context->Program->Shader->IO ()
attachShader = ffi "(function(ctx, prog, shader) {ctx.attachShader(prog, shader);})"

createProgram::Context->IO Program
createProgram = ffi "(function(ctx) {return ctx.createProgram();})"

linkProgram::Context->Program->IO ()
linkProgram = ffi "(function(ctx, prog) {ctx.linkProgram(prog);})"

useProgram::Context->Program->IO ()
useProgram = ffi "(function(ctx, prog) {ctx.useProgram(prog);})"

getAttribLocation::Context->Program->String->IO AttribLocation
getAttribLocation = ffi "(function(ctx, prog, name) {return ctx.getAttribLocation(prog, name);})"

enableVertexAttribArray::Context->AttribLocation->IO ()
enableVertexAttribArray = ffi "(function(ctx, attr) {ctx.enableVertexAttribArray(attr);})"

vertexAttribPointer::Context->AttribLocation->Int->AttribType->Bool->Int->Int->IO ()
vertexAttribPointer ctx loc size typ = vertexAttribPointer' ctx loc size typ'
  where
    vertexAttribPointer'::Context->AttribLocation->Int->AttribType'->Bool->Int->Int->IO ()
    vertexAttribPointer' = ffi "(function(ctx, loc, size, typ, norm, stride, offset) {ctx.vertexAttribPointer(loc, size, typ, norm, stride, offset);})"

    typ' = unsafePerformIO $ idxAttribType ctx strtyp
      where
        idxAttribType::Context->String->IO AttribType'
        idxAttribType = ffi "(function(ctx, typ) {return ctx[typ];})"
        strtyp = case typ of
          Byte -> "BYTE"
          Short -> "SHORT"
          UnsignedByte -> "UNSIGNED_BYTE"
          UnsignedShort -> "UNSIGNED_SHORT"
          Fixed -> "FIXED"
          _ -> "FLOAT"

getUniformLocation::Context->Program->String->IO UniformLocation
getUniformLocation = ffi "(function(ctx, prog, name) {return ctx.getUniformLocation(prog, name);})"

createBuffer::Context->IO Buffer
createBuffer = ffi "(function(ctx) {return ctx.createBuffer();})"

idxBufferType::Context->BufferType->BufferType'
idxBufferType ctx typ = unsafePerformIO $ idxBufferType' ctx typ'
  where
    idxBufferType'::Context->String->IO BufferType'
    idxBufferType' = ffi "(function(ctx, typ) {return ctx[typ];})"

    typ' = case typ of
      ArrayBuffer -> "ARRAY_BUFFER"
      _ -> "ELEMENT_ARRAY_BUFFER"

bindBuffer::Context->BufferType->Buffer->IO ()
bindBuffer ctx typ = bindBuffer' ctx typ'
  where
    bindBuffer'::Context->BufferType'->Buffer->IO ()
    bindBuffer' = ffi "(function(ctx, typ, buf) {ctx.bindBuffer(typ, buf);})"

    typ' = idxBufferType ctx typ

bufferData::(Unboxable a)=>Context->BufferType->[a]->BufferUsage->IO ()
bufferData ctx typ arr use = bufferData' ctx typ' (TA.fromList arr) use'
  where
    typ' = idxBufferType ctx typ

    use' = unsafePerformIO $ idxBufferUse ctx usestr

    idxBufferUse::Context->String->IO BufferUsage'
    idxBufferUse = ffi "(function(ctx, use) {return ctx[use];})"

    usestr = case use of
      StaticDraw -> "STATIC_DRAW"
      StreamDraw -> "STREAM_DRAW"
      DynamicDraw -> "DYNAMIC_DRAW"

    bufferData'::Context->BufferType'->TypedArray a->BufferUsage'->IO ()
    bufferData' = ffi "(function(ctx, typ, arr, use) {ctx.bufferData(typ, arr, use);})"

uniformMatrix4fv::Context->UniformLocation->Mat44 Float->IO ()
uniformMatrix4fv ctx loc mat = uniformMatrix4fv' ctx loc (matToList . transpose $ mat)
  where
    uniformMatrix4fv'::Context->UniformLocation->[Float]->IO ()
    uniformMatrix4fv' = ffi "(function(ctx, uni, vals) {ctx.uniformMatrix4fv(uni, false, vals);})"

clear::Context->[ClearBit]->IO ()
clear ctx bits = clear' ctx bits'
  where
    clear'::Context->ClearBit'->IO ()
    clear' = ffi "(function(ctx, bits) {ctx.clear(bits);})"

    orClearBit::ClearBit'->ClearBit'->IO ClearBit'
    orClearBit = ffi "(function(a, b) {return a | b;})"

    bits' = unsafePerformIO $ foldrM orClearBit (unsafePerformIO $ ffi "0"::ClearBit') $ idxClearBit ctx <$> bits

    idxClearBit::Context->ClearBit->ClearBit'
    idxClearBit ctx bit = unsafePerformIO $ idxClearBit' ctx bitstr
      where
        idxClearBit'::Context->String->IO ClearBit'
        idxClearBit' = ffi "(function(ctx, name) {return ctx[name];})"

        bitstr = case bit of
          ColorBufferBit -> "COLOR_BUFFER_BIT"
          DepthBufferBit -> "DEPTH_BUFFER_BIT"
          _ -> "STENCIL_BUFFER_BIT"


viewport::Context->Int->Int->Int->Int->IO ()
viewport = ffi "(function(ctx, x, y, w, h) {ctx.viewport(x, y, w, h);})"

drawArrays::Context->DrawMode->Int->Int->IO ()
drawArrays ctx mode = drawArrays' ctx mode'
  where
    drawArrays'::Context->DrawMode'->Int->Int->IO ()
    drawArrays' = ffi "(function(ctx, mode, start, count) {ctx.drawArrays(mode, start, count);})"

    mode' = idxDrawMode ctx mode

    idxDrawMode::Context->DrawMode->DrawMode'
    idxDrawMode ctx mode = unsafePerformIO $ idxDrawMode' ctx strmode
      where
        idxDrawMode'::Context->String->IO DrawMode'
        idxDrawMode' = ffi "(function(ctx, name) {return ctx[name];})"

        strmode = case mode of
          Points -> "POINTS"
          LineStrip -> "LINE_STRIP"
          LineLoop -> "LINE_LOOP"
          Lines -> "LINES"
          TriangleStrip -> "TRIANGLE_STRIP"
          TriangleFan -> "TRIANGLE_FAN"
          _ -> "TRIANGLES"

enable::Context->Ability->IO ()
enable ctx abil = enable' ctx abil'
  where
    enable'::Context->Ability'->IO ()
    enable' = ffi "(function(ctx, abil) {ctx.enable(abil);})"

    abil' = unsafePerformIO $ indexAbility ctx strabil
      where
        indexAbility::Context->String->IO Ability'
        indexAbility = ffi "(function(ctx, name) {return ctx[name];})"

        strabil = case abil of
          Blend -> "BLEND"
          CullFace -> "CULL_FACE"
          DepthTest -> "DEPTH_TEST"
          Dither -> "DITHER"
          PolygonOffsetFill -> "POLYGON_OFFSET_FILL"
          SampleAlphaToCoverage -> "SAMPLE_ALPHA_TO_COVERAGE"
          SampleCoverage -> "SAMPLE_COVERAGE"
          ScissorTest -> "SCISSOR_TEST"
          StencilTest -> "STENCIL_TEST"

clearColor::Context->Double->Double->Double->Double->IO ()
clearColor = ffi "(function(ctx, r, g, b, a) {ctx.clearColor(r, g, b, a);})"
