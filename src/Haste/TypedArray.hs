{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}

module Haste.TypedArray where

import Data.Int
import Data.Word

import Haste.Foreign
import Haste.Prim
import System.IO.Unsafe

-- newtype ArrayBuffer = ArrayBuffer JSAny deriving (Pack, Unpack)

newtype TypedArray e = TypedArray JSAny deriving (Pack, Unpack)

class Unboxable e where
  fromList::[e]->TypedArray e

instance Unboxable Int8 where
  fromList = unsafePerformIO . ffi "(function(lst) {return Int8Array(lst);})"

instance Unboxable Int16 where
  fromList = unsafePerformIO . ffi "(function(lst) {return Int16Array(lst);})"

instance Unboxable Int32 where
  fromList = unsafePerformIO . ffi "(function(lst) {return Int32Array(lst);})"

instance Unboxable Word8 where
  fromList = unsafePerformIO . ffi "(function(lst) {return Uint8Array(lst);})"

instance Unboxable Word16 where
  fromList = unsafePerformIO . ffi "(function(lst) {return Uint16Array(lst);})"

instance Unboxable Word32 where
  fromList = unsafePerformIO . ffi "(function(lst) {return Uint32Array(lst);})"

instance Unboxable Float where
  fromList = unsafePerformIO . ffi "(function(lst) {return Float32Array(lst);})"

instance Unboxable Double where
  fromList = unsafePerformIO . ffi "(function(lst) {return Float64Array(lst);})"
