{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, TypeFamilies, FlexibleContexts #-}

module Haste.Graphics.WebGL.Arrays where

import Haste.Foreign
import Haste.Prim
import Foreign.Ptr

newtype ArrayBuffer = ArrayBuffer JSAny deriving (Pack, Unpack)

newArrayBuffer::Int->IO ArrayBuffer
newArrayBuffer = ffi "(function(byteLength) {return ArrayBuffer(byteLength);})"

class (Pack a, Unpack a)=>TypedArray a where
  type EltType :: *
  newSizedArray::Int->IO a
  copyTypedArray::TypedArray b=>b->IO a
  fromJSArray::Unpack EltType=>[EltType]->IO a
  fromArrayBuffer::Int->Int->ArrayBuffer->IO a

  byteLength::a->IO Int
  byteLength = ffi "(function(arr) {return arr.byteLength;})"
  bytesPerElement::a->IO Int
  bytesPerElement = ffi "(function(arr) {return arr.BYTES_PER_ELEMENT;})"

  getIndex::Pack EltType=>a->Int->IO (EltType)
  getIndex = ffi "(function(arr, idx) {return arr[idx];})"
  setIndex::Unpack EltType=>a->Int->(EltType)->IO ()
  setIndex = ffi "(function(arr, idx, val) {arr[idx] = val;})"

newtype Int8Array = Int8Array JSAny deriving (Pack, Unpack)

instance TypedArray Int8Array where
  type EltType = Int
  newSizedArray = ffi "(function(size) {return Int8Array(size);})"
  copyTypedArray = ffi "(function(other) {return Int8Array(other);})"
  fromJSArray = ffi "(function(other) {return Int8Array(other);})"
  fromArrayBuffer = ffi "(function(byteOffset, length, buffer) {return Int8Array(buffer, byteOffset, length);})"

newtype Int16Array = Int16Array JSAny deriving (Pack, Unpack)

instance TypedArray Int16Array where
  type EltType = Int
  newSizedArray = ffi "(function(size) {return Int16Array(size);})"
  copyTypedArray = ffi "(function(other) {return Int16Array(other);})"
  fromJSArray = ffi "(function(other) {return Int16Array(other);})"
  fromArrayBuffer = ffi "(function(byteOffset, length, buffer) {return Int16Array(buffer, byteOffset, length);})"

newtype Int32Array = Int32Array JSAny deriving (Pack, Unpack)

instance TypedArray Int32Array where
  type EltType = Int
  newSizedArray = ffi "(function(size) {return Int32Array(size);})"
  copyTypedArray = ffi "(function(other) {return Int32Array(other);})"
  fromJSArray = ffi "(function(other) {return Int32Array(other);})"
  fromArrayBuffer = ffi "(function(byteOffset, length, buffer) {return Int32Array(buffer, byteOffset, length);})"

newtype Int64Array = Int64Array JSAny deriving (Pack, Unpack)

instance TypedArray Int64Array where
  type EltType = Int
  newSizedArray = ffi "(function(size) {return Int64Array(size);})"
  copyTypedArray = ffi "(function(other) {return Int64Array(other);})"
  fromJSArray = ffi "(function(other) {return Int64Array(other);})"
  fromArrayBuffer = ffi "(function(byteOffset, length, buffer) {return Int64Array(buffer, byteOffset, length);})"

newtype Uint8Array = Uint8Array JSAny deriving (Pack, Unpack)

instance TypedArray Uint8Array where
  type EltType = Int
  newSizedArray = ffi "(function(size) {return Uint8Array(size);})"
  copyTypedArray = ffi "(function(other) {return Uint8Array(other);})"
  fromJSArray = ffi "(function(other) {return Uint8Array(other);})"
  fromArrayBuffer = ffi "(function(byteOffset, length, buffer) {return Uint8Array(buffer, byteOffset, length);})"

newtype Uint16Array = Uint16Array JSAny deriving (Pack, Unpack)

instance TypedArray Uint16Array where
  type EltType = Int
  newSizedArray = ffi "(function(size) {return Uint16Array(size);})"
  copyTypedArray = ffi "(function(other) {return Uint16Array(other);})"
  fromJSArray = ffi "(function(other) {return Uint16Array(other);})"
  fromArrayBuffer = ffi "(function(byteOffset, length, buffer) {return Uint16Array(buffer, byteOffset, length);})"

newtype Uint32Array = Uint32Array JSAny deriving (Pack, Unpack)

instance TypedArray Uint32Array where
  type EltType = Int
  newSizedArray = ffi "(function(size) {return Uint32Array(size);})"
  copyTypedArray = ffi "(function(other) {return Uint32Array(other);})"
  fromJSArray = ffi "(function(other) {return Uint32Array(other);})"
  fromArrayBuffer = ffi "(function(byteOffset, length, buffer) {return Uint32Array(buffer, byteOffset, length);})"

newtype Float32Array = Float32Array JSAny deriving (Pack, Unpack)

instance TypedArray Float32Array where
  type EltType = Int
  newSizedArray = ffi "(function(size) {return Float32Array(size);})"
  copyTypedArray = ffi "(function(other) {return Float32Array(other);})"
  fromJSArray = ffi "(function(other) {return Float32Array(other);})"
  fromArrayBuffer = ffi "(function(byteOffset, length, buffer) {return Float32Array(buffer, byteOffset, length);})"
