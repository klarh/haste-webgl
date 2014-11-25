{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, TypeFamilies, FlexibleContexts #-}

module Haste.Graphics.WebGL.Arrays where

import Control.Monad (liftM2)
import Haste.Foreign
import Haste.Prim
import Foreign.Ptr

newtype ArrayBuffer = ArrayBuffer JSAny deriving (Pack, Unpack)

newArrayBuffer::Int->IO ArrayBuffer
newArrayBuffer = ffi "(function(byteLength) {return ArrayBuffer(byteLength);})"

arrayBufferLength::ArrayBuffer->IO Int
arrayBufferLength = ffi "(function(buf) {return buf.length;})"

class (Pack a, Unpack a)=>TypedArray a where
  type EltType a
  newSizedArray::Int->IO a
  copyTypedArray::TypedArray b=>b->IO a
  fromList::Unpack (EltType a)=>[EltType a]->IO a
  fromArrayBufferSized::Int->Int->ArrayBuffer->IO a
  fromArrayBuffer::ArrayBuffer->IO a

  byteLength::a->IO Int
  byteLength = ffi "(function(arr) {return arr.byteLength;})"
  bytesPerElement::a->IO Int
  bytesPerElement = ffi "(function(arr) {return arr.BYTES_PER_ELEMENT;})"
  arrayLength::a->IO Int
  arrayLength arr = liftM2 quot (byteLength arr) (bytesPerElement arr)

  getIndex::Pack (EltType a)=>a->Int->IO (EltType a)
  getIndex = ffi "(function(arr, idx) {return arr[idx];})"
  setIndex::Unpack (EltType a)=>a->Int->EltType a->IO ()
  setIndex = ffi "(function(arr, idx, val) {arr[idx] = val;})"
  toList::Pack (EltType a)=>a->IO [EltType a]
  toList arr = do
    length <- arrayLength arr
    mapM (getIndex arr) [0..length - 1]

newtype Int8Array = Int8Array JSAny deriving (Pack, Unpack)

instance TypedArray Int8Array where
  type (EltType Int8Array) = Int
  newSizedArray = ffi "(function(size) {return new Int8Array(size);})"
  copyTypedArray = ffi "(function(other) {return new Int8Array(other);})"
  fromList = ffi "(function(other) {return new Int8Array(other);})"
  fromArrayBufferSized = ffi "(function(byteOffset, length, buffer) {return new Int8Array(buffer, byteOffset, length);})"
  fromArrayBuffer = ffi "(function(buffer) {return new Int8Array(buffer);})"

newtype Int16Array = Int16Array JSAny deriving (Pack, Unpack)

instance TypedArray Int16Array where
  type (EltType Int16Array) = Int
  newSizedArray = ffi "(function(size) {return new Int16Array(size);})"
  copyTypedArray = ffi "(function(other) {return new Int16Array(other);})"
  fromList = ffi "(function(other) {return new Int16Array(other);})"
  fromArrayBufferSized = ffi "(function(byteOffset, length, buffer) {return new Int16Array(buffer, byteOffset, length);})"
  fromArrayBuffer = ffi "(function(buffer) {return new Int16Array(buffer);})"

newtype Int32Array = Int32Array JSAny deriving (Pack, Unpack)

instance TypedArray Int32Array where
  type (EltType Int32Array) = Int
  newSizedArray = ffi "(function(size) {return new Int32Array(size);})"
  copyTypedArray = ffi "(function(other) {return new Int32Array(other);})"
  fromList = ffi "(function(other) {return new Int32Array(other);})"
  fromArrayBufferSized = ffi "(function(byteOffset, length, buffer) {return new Int32Array(buffer, byteOffset, length);})"
  fromArrayBuffer = ffi "(function(buffer) {return new Int32Array(buffer);})"

newtype Int64Array = Int64Array JSAny deriving (Pack, Unpack)

instance TypedArray Int64Array where
  type (EltType Int64Array) = Int
  newSizedArray = ffi "(function(size) {return new Int64Array(size);})"
  copyTypedArray = ffi "(function(other) {return new Int64Array(other);})"
  fromList = ffi "(function(other) {return new Int64Array(other);})"
  fromArrayBufferSized = ffi "(function(byteOffset, length, buffer) {return new Int64Array(buffer, byteOffset, length);})"
  fromArrayBuffer = ffi "(function(buffer) {return new Int64Array(buffer);})"

newtype Uint8Array = Uint8Array JSAny deriving (Pack, Unpack)

instance TypedArray Uint8Array where
  type (EltType Uint8Array) = Int
  newSizedArray = ffi "(function(size) {return new Uint8Array(size);})"
  copyTypedArray = ffi "(function(other) {return new Uint8Array(other);})"
  fromList = ffi "(function(other) {return new Uint8Array(other);})"
  fromArrayBufferSized = ffi "(function(byteOffset, length, buffer) {return new Uint8Array(buffer, byteOffset, length);})"
  fromArrayBuffer = ffi "(function(buffer) {return new Uint8Array(buffer);})"

newtype Uint16Array = Uint16Array JSAny deriving (Pack, Unpack)

instance TypedArray Uint16Array where
  type (EltType Uint16Array) = Int
  newSizedArray = ffi "(function(size) {return new Uint16Array(size);})"
  copyTypedArray = ffi "(function(other) {return new Uint16Array(other);})"
  fromList = ffi "(function(other) {return new Uint16Array(other);})"
  fromArrayBufferSized = ffi "(function(byteOffset, length, buffer) {return new Uint16Array(buffer, byteOffset, length);})"
  fromArrayBuffer = ffi "(function(buffer) {return new Uint16Array(buffer);})"

newtype Uint32Array = Uint32Array JSAny deriving (Pack, Unpack)

instance TypedArray Uint32Array where
  type (EltType Uint32Array) = Int
  newSizedArray = ffi "(function(size) {return new Uint32Array(size);})"
  copyTypedArray = ffi "(function(other) {return new Uint32Array(other);})"
  fromList = ffi "(function(other) {return new Uint32Array(other);})"
  fromArrayBufferSized = ffi "(function(byteOffset, length, buffer) {return new Uint32Array(buffer, byteOffset, length);})"
  fromArrayBuffer = ffi "(function(buffer) {return new Uint32Array(buffer);})"

newtype Float32Array = Float32Array JSAny deriving (Pack, Unpack)

instance TypedArray Float32Array where
  type (EltType Float32Array) = Double
  newSizedArray = ffi "(function(size) {return new Float32Array(size);})"
  copyTypedArray = ffi "(function(other) {return new Float32Array(other);})"
  fromList = ffi "(function(other) {return new Float32Array(other);})"
  fromArrayBufferSized = ffi "(function(byteOffset, length, buffer) {return new Float32Array(buffer, byteOffset, length);})"
  fromArrayBuffer = ffi "(function(buffer) {return new Float32Array(buffer);})"
