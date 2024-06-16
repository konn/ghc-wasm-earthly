{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnliftedDatatypes #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module GHC.Wasm.Object.Builtins.Buffer (
  JSByteArrayClass,
  JSByteArray,
  usePrimArrayAsJSByteArray,
  usePrimVectorAsJSByteArray,
  toPrimArray,
  toByteArray,
  toByteString,
  Int8Array,
  Uint8Array,
  Int16Array,
  Uint16Array,
  Int32Array,
  Uint32Array,
  BigInt64Array,
  BigUint64Array,
  Float32Array,
  Float64Array,
  JSByteArrayElement (..),
) where

import Control.Arrow ((>>>))
import Data.Array.Byte (ByteArray (..))
import qualified Data.ByteString as BS
import Data.ByteString.Short (ShortByteString (..))
import qualified Data.ByteString.Short as BS
import Data.Kind (Type)
import Data.Primitive (PrimArray (..), copyPrimArray, copyPrimArrayToPtr, copyPtrToMutablePrimArray, newPrimArray, runPrimArray, sizeofPrimArray)
import Data.Primitive.Types (Prim)
import qualified Data.Vector.Primitive as PV
import Foreign
import GHC.Wasm.Object.Core

toByteString :: (JSByteArrayElement a) => JSByteArray a -> BS.ByteString
toByteString = BS.fromShort . ShortByteString . toByteArray

toByteArray :: (JSByteArrayElement a) => JSByteArray a -> ByteArray
toByteArray = toPrimArray >>> \(PrimArray ba) -> ByteArray ba

toPrimArray :: (JSByteArrayElement a) => JSByteArray a -> PrimArray a
toPrimArray ba = runPrimArray do
  parr <- newPrimArray (elemLength ba)
  copyPtrToMutablePrimArray parr 0 (byteOffset ba) (elemLength ba)
  pure parr

usePrimArrayAsJSByteArray :: (JSByteArrayElement a) => PrimArray a -> (JSByteArray a -> IO b) -> IO b
usePrimArrayAsJSByteArray parr act = do
  allocaArray (sizeofPrimArray parr) \ptr -> do
    copyPrimArrayToPtr ptr parr 0 (sizeofPrimArray parr)
    act =<< allocateArray ptr (sizeofPrimArray parr)

usePrimVectorAsJSByteArray :: (JSByteArrayElement a) => PV.Vector a -> (JSByteArray a -> IO b) -> IO b
usePrimVectorAsJSByteArray (PV.Vector off len (ByteArray ba)) f = do
  let pa = PrimArray ba
  if off == 0 && len == sizeofPrimArray pa
    then usePrimArrayAsJSByteArray pa f
    else
      let pa' = runPrimArray do
            ma <- newPrimArray len
            copyPrimArray ma 0 pa off len
            pure ma
       in usePrimArrayAsJSByteArray pa' f

type data JSByteArrayClass :: Type -> Prototype

type JSByteArray a = JSObject (JSByteArrayClass a)

type Int8Array = JSByteArray Int8

type Uint8Array = JSByteArray Word8

type Int16Array = JSByteArray Int16

type Uint16Array = JSByteArray Word16

type Int32Array = JSByteArray Int32

type Uint32Array = JSByteArray Word32

type BigInt64Array = JSByteArray Int64

type BigUint64Array = JSByteArray Word64

type Float32Array = JSByteArray Float

type Float64Array = JSByteArray Double

class (Storable a, Prim a) => JSByteArrayElement a where
  allocateArray :: Ptr a -> Int -> IO (JSByteArray a)
  byteLength :: JSByteArray a -> Int
  byteLength = js_buffer_byteLength
  byteOffset :: JSByteArray a -> Ptr a
  byteOffset = js_buffer_byteOffset
  elemLength :: JSByteArray a -> Int
  elemLength = js_buffer_elemLength

instance JSByteArrayElement Int8 where
  allocateArray = js_alloc_int8

instance JSByteArrayElement Int16 where
  allocateArray = js_alloc_int16

instance JSByteArrayElement Int32 where
  allocateArray = js_alloc_int32

instance JSByteArrayElement Int64 where
  allocateArray = js_alloc_bigint64

instance JSByteArrayElement Word8 where
  allocateArray = js_alloc_uint8

instance JSByteArrayElement Word16 where
  allocateArray = js_alloc_uint16

instance JSByteArrayElement Word32 where
  allocateArray = js_alloc_uint32

instance JSByteArrayElement Word64 where
  allocateArray = js_alloc_biguint64

instance JSByteArrayElement Float where
  allocateArray = js_alloc_float32

instance JSByteArrayElement Double where
  allocateArray = js_alloc_float64

foreign import javascript unsafe "new Uint8Array(__exports.memory.buffer, $1, $2)"
  js_alloc_uint8 :: Ptr Word8 -> Int -> IO (JSByteArray Word8)

foreign import javascript unsafe "new Int8Array(__exports.memory.buffer, $1, $2)"
  js_alloc_int8 :: Ptr Int8 -> Int -> IO (JSByteArray Int8)

foreign import javascript unsafe "new Uint16Array(__exports.memory.buffer, $1, $2)"
  js_alloc_uint16 :: Ptr Word16 -> Int -> IO (JSByteArray Word16)

foreign import javascript unsafe "new Int16Array(__exports.memory.buffer, $1, $2)"
  js_alloc_int16 :: Ptr Int16 -> Int -> IO (JSByteArray Int16)

foreign import javascript unsafe "new Uint32Array(__exports.memory.buffer, $1, $2)"
  js_alloc_uint32 :: Ptr Word32 -> Int -> IO (JSByteArray Word32)

foreign import javascript unsafe "new Int32Array(__exports.memory.buffer, $1, $2)"
  js_alloc_int32 :: Ptr Int32 -> Int -> IO (JSByteArray Int32)

foreign import javascript unsafe "new BigInt64Array(__exports.memory.buffer, $1, $2)"
  js_alloc_bigint64 :: Ptr Int64 -> Int -> IO (JSByteArray Int64)

foreign import javascript unsafe "new BigUint64Array(__exports.memory.buffer, $1, $2)"
  js_alloc_biguint64 :: Ptr Word64 -> Int -> IO (JSByteArray Word64)

foreign import javascript unsafe "new Float32Array(__exports.memory.buffer, $1, $2)"
  js_alloc_float32 :: Ptr Float -> Int -> IO (JSByteArray Float)

foreign import javascript unsafe "new Float64Array(__exports.memory.buffer, $1, $2)"
  js_alloc_float64 :: Ptr Double -> Int -> IO (JSByteArray Double)

foreign import javascript safe "$1.byteOffset"
  js_buffer_byteOffset :: JSByteArray a -> Ptr a

foreign import javascript safe "$1.byteLength"
  js_buffer_byteLength :: JSByteArray a -> Int

foreign import javascript safe "$1.length"
  js_buffer_elemLength :: JSByteArray a -> Int
