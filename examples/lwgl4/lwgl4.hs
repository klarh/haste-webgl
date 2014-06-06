{-# LANGUAGE EmptyDataDecls, OverloadedStrings #-}

import Control.Monad
import Data.IORef
import Data.Vec
import Data.Word
import Haste
import Haste.Foreign
import Haste.DOM
import Haste.Graphics.WebGL

fragmentShaderText = unlines [
  "precision mediump float;",
  "varying vec4 vColor;",
  "void main(void) {",
  "    gl_FragColor = vColor;",
  "}"]

vertexShaderText = unlines [
  "attribute vec3 aVertexPosition;",
  "attribute vec4 aVertexColor;",
  "uniform mat4 uMVMatrix;",
  "uniform mat4 uPMatrix;",
  "varying vec4 vColor;",
  "void main(void) {",
  "  gl_Position = uPMatrix * uMVMatrix * vec4(aVertexPosition, 1.0);",
  "  vColor = aVertexColor;",
  "}"]

getShader::Context->ShaderType->String->IO Shader
getShader gl typ src = do
  result <- createShader gl typ
  shaderSource gl result src
  compileShader gl result
  return result

initShaders::Context->IO (UniformLocation, UniformLocation, AttribLocation, AttribLocation)
initShaders gl = do
  fragmentShader <- getShader gl FragmentShader fragmentShaderText
  vertexShader <- getShader gl VertexShader vertexShaderText
  shaderProgram <- createProgram gl
  attachShader gl shaderProgram vertexShader
  attachShader gl shaderProgram fragmentShader
  linkProgram gl shaderProgram

  useProgram gl shaderProgram

  vertPos <- getAttribLocation gl shaderProgram "aVertexPosition"
  enableVertexAttribArray gl vertPos
  vertCol <- getAttribLocation gl shaderProgram "aVertexColor"
  enableVertexAttribArray gl vertCol

  pmatUniform <- getUniformLocation gl shaderProgram "uPMatrix"
  mvmatUniform <- getUniformLocation gl shaderProgram "uMVMatrix"

  return (pmatUniform, mvmatUniform, vertPos, vertCol)

setMatrixUniforms::Context->(UniformLocation, UniformLocation)->(Mat44 Float, Mat44 Float)->IO ()
setMatrixUniforms gl (pIdx, mvIdx) (pMat, mvMat) = do
  uniformMatrix4fv gl pIdx pMat
  uniformMatrix4fv gl mvIdx mvMat

initBuffers::Context->IO (Buffer, Buffer, Buffer, Buffer, Buffer)
initBuffers gl = do
  pyramidVertsBuffer <- createBuffer gl
  bindBuffer gl ArrayBuffer pyramidVertsBuffer

  let pyramidVerts = [0, 1, 0, -1, -1, 1, 1, -1, 1,
                      0, 1, 0, 1, -1, 1, 1, -1, -1,
                      0, 1, 0, 1, -1, -1, -1, -1, -1,
                      0, 1, 0, -1, -1, -1, -1, -1, 1]::[Float]

  bufferData gl ArrayBuffer pyramidVerts StaticDraw

  pyramidColorBuffer <- createBuffer gl
  bindBuffer gl ArrayBuffer pyramidColorBuffer

  let pyramidColors = [1, 0, 0, 1, 0, 1, 0, 1, 0, 0, 1, 1,
                       1, 0, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1,
                       1, 0, 0, 1, 0, 1, 0, 1, 0, 0, 1, 1,
                       1, 0, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1]::[Float]

  bufferData gl ArrayBuffer pyramidColors StaticDraw

  cubeVertsBuffer <- createBuffer gl
  bindBuffer gl ArrayBuffer cubeVertsBuffer

  let cubeVerts = [-1, -1, 1, 1, -1, 1, 1, 1, 1, -1, 1, 1,
                   -1, -1, -1, -1, 1, -1, 1, 1, -1, 1, -1, -1,
                   -1, 1, -1, -1, 1, 1, 1, 1, 1, 1, 1, -1,
                   -1, -1, -1, 1, -1, -1, 1, -1, 1, -1, -1, 1,
                   1, -1, -1, 1, 1, -1, 1, 1, 1, 1, -1, 1,
                   -1, -1, -1, -1, -1, 1, -1, 1, 1, -1, 1, -1]::[Float]
  bufferData gl ArrayBuffer cubeVerts StaticDraw

  cubeColorBuffer <- createBuffer gl
  bindBuffer gl ArrayBuffer cubeColorBuffer

  let faceColors = [[1, 0, 0, 1], [1, 1, 0, 1],
                    [0, 1, 0, 1], [1, 0.5, 0.5, 1],
                    [1, 0, 1, 1], [0, 0, 1, 1]]::[[Float]]
      cubeColors = concat . concatMap (Prelude.take 4 . repeat) $ faceColors
  bufferData gl ArrayBuffer cubeColors StaticDraw

  cubeIndexBuffer <- createBuffer gl
  bindBuffer gl ElementArrayBuffer cubeIndexBuffer

  let cubeIndices = [0, 1, 2, 0, 2, 3,
                     4, 5, 6, 4, 6, 7,
                     8, 9, 10, 8, 10, 11,
                     12, 13, 14, 12, 14, 15,
                     16, 17, 18, 16, 18, 19,
                     20, 21, 22, 20, 22, 23]::[Word16]
  bufferData gl ElementArrayBuffer cubeIndices StaticDraw

  return (pyramidVertsBuffer, pyramidColorBuffer, cubeVertsBuffer, cubeColorBuffer, cubeIndexBuffer)

drawScene::Context->(UniformLocation, UniformLocation)->Int->
           (Buffer, Buffer, Buffer, Buffer, Buffer)->(AttribLocation, AttribLocation)->IO ()
drawScene gl (pIdx, mvIdx) time buffers attribs = do
  let (pyramidVertsBuffer, pyramidColorBuffer, cubeVertsBuffer, cubeColorBuffer, cubeIndexBuffer) = buffers
      (posAttrib, colorAttrib) = attribs
      rPyramid = (fromIntegral time/100)
      rCube = (3.5*(fromIntegral time)/100)

  viewport gl 0 0 640 480
  clear gl [ColorBufferBit, DepthBufferBit]

  let pmat =  perspective 0.1 100 (pi/4) (640/480)
      mvmat = translation (negate 1.5:.0:.negate 8:.())
      rotated = mvmat `multmm` rotationY (rPyramid*pi/180)

  bindBuffer gl ArrayBuffer pyramidVertsBuffer
  vertexAttribPointer gl posAttrib 3 Float False 0 0

  bindBuffer gl ArrayBuffer pyramidColorBuffer
  vertexAttribPointer gl colorAttrib 4 Float False 0 0

  setMatrixUniforms gl (pIdx, mvIdx) (pmat, rotated)
  drawArrays gl Triangles 0 (4*3)

  let mvmat = translation (1.5:.0:.negate 8:.())
      rotated = mvmat `multmm` rotationVec (normalize (1:.1:.1:.())) (rCube*pi/180)

  bindBuffer gl ArrayBuffer cubeVertsBuffer
  vertexAttribPointer gl posAttrib 3 Float False 0 0

  bindBuffer gl ArrayBuffer cubeColorBuffer
  vertexAttribPointer gl colorAttrib 4 Float False 0 0

  bindBuffer gl ElementArrayBuffer cubeIndexBuffer

  setMatrixUniforms gl (pIdx, mvIdx) (pmat, rotated)
  drawElements gl Triangles 36 UShortElt 0

forever delay m = m >> setTimeout delay (Main.forever delay m)

main = do
  let root = documentBody
  canvas <- newElem "canvas"
  setAttr canvas "width" "640"
  setAttr canvas "height" "480"
  addChild canvas root

  gl <- getContext canvas "webgl"
  (pmUni, mvUni, posAttr, colAttr) <- initShaders gl
  buffers <- initBuffers gl

  enable gl DepthTest

  ref <- newIORef (0, 0)::IO (IORef (Float, Float))

  Main.forever (floor $ 1000/60) $ do
    clearColor gl 0 0 0 1
    t <- ffi "(function() {return (new Date().getTime());})" :: IO Int
    drawScene gl (pmUni, mvUni) t buffers (posAttr, colAttr)

  return ()
