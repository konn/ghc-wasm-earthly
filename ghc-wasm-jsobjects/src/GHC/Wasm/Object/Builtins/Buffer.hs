{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnliftedDatatypes #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module GHC.Wasm.Object.Builtins.Buffer (
  -- * Raw, delegatable array-bufer.
  ArrayBufferClass,
  ArrayBuffer,
  fromArrayBuffer,
  newArrayBuffer,

  -- ** Common definitions
  BufferSourceClass,
  BufferSource,
  ArrayBufferViewClass,
  ArrayBufferView,

  -- * Byte array of various types.
  JSByteArrayClass,
  JSByteArray,
  byteArrayElementLength,
  byteArrayByteLength,
  useByteStringAsJSByteArray,
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

  -- * DataView
  DataViewClass,
  DataView,
  toDataView,
  dataViewByteLength,

  -- ** getters
  getInt8,
  getUint8,
  getInt16,
  getUint16,
  getInt32,
  getUint32,
  getBigInt64,
  getBigUint64,
  getFloat32,
  getFloat64,

  -- ** setters
  setInt8,
  setUint8,
  setInt16,
  setUint16,
  setInt32,
  setUint32,
  setBigInt64,
  setBigUint64,
  setFloat32,
  setFloat64,

  -- * Deprecated
  SharedArrayBuffer,
  SharedArrayBufferClass,
) where

import Control.Arrow ((>>>))
import Control.Monad.ST.Unsafe (unsafeIOToST)
import Data.Array.Byte (ByteArray (..))
import qualified Data.ByteString as BS
import Data.ByteString.Short (ShortByteString (..))
import qualified Data.ByteString.Short as BS
import qualified Data.ByteString.Short as SBS
import Data.Kind (Type)
import Data.Primitive (PrimArray (..), copyPrimArray, copyPrimArrayToPtr, mutablePrimArrayContents, newPinnedPrimArray, newPrimArray, runPrimArray, sizeofPrimArray)
import Data.Primitive.Types (Prim)
import qualified Data.Vector.Primitive as PV
import Foreign
import GHC.Wasm.Object.Core

type data ArrayBufferClass :: Prototype

type instance SuperclassOf ArrayBufferClass = 'Nothing

type ArrayBuffer = JSObject ArrayBufferClass

type data SharedArrayBufferClass :: Prototype

type instance SuperclassOf SharedArrayBufferClass = 'Nothing

type SharedArrayBuffer = JSObject SharedArrayBufferClass

type ArrayBufferViewClass =
  UnionClass
    '[ JSByteArrayClass Int8
     , JSByteArrayClass Int16
     , JSByteArrayClass Int32
     , JSByteArrayClass Int64
     , JSByteArrayClass Word8
     , JSByteArrayClass Word16
     , JSByteArrayClass Word32
     , JSByteArrayClass Word64
     , JSByteArrayClass Float
     , JSByteArrayClass Double
     , DataViewClass
     ]

type ArrayBufferView = JSObject ArrayBufferViewClass

type BufferSourceClass = UnionClass '[ArrayBufferClass, ArrayBufferViewClass]

type BufferSource = JSObject BufferSourceClass

fromArrayBuffer :: (JSByteArrayElement a) => ArrayBuffer -> IO (JSByteArray a)
fromArrayBuffer = fromArrayBuffer_

foreign import javascript unsafe "new ArrayBuffer($1)"
  newArrayBuffer :: Int -> IO ArrayBuffer

toByteString :: (JSByteArrayElement a) => JSByteArray a -> BS.ByteString
toByteString = BS.fromShort . ShortByteString . toByteArray

toByteArray :: (JSByteArrayElement a) => JSByteArray a -> ByteArray
toByteArray = toPrimArray >>> \(PrimArray ba) -> ByteArray ba

toPrimArray :: (JSByteArrayElement a) => JSByteArray a -> PrimArray a
toPrimArray ba = runPrimArray do
  let !len = elemLength ba
  parr <- newPinnedPrimArray len
  barr <- unsafeIOToST $ allocateArray (mutablePrimArrayContents parr) len
  unsafeIOToST $ js_copy_into barr ba
  pure parr

foreign import javascript unsafe "$1.set($2)"
  js_copy_into :: JSByteArray a -> JSByteArray a -> IO ()

useByteStringAsJSByteArray ::
  (JSByteArrayElement a) =>
  BS.ByteString ->
  (JSByteArray a -> IO r) ->
  IO r
useByteStringAsJSByteArray bs f =
  case SBS.toShort bs of
    SBS ba -> usePrimArrayAsJSByteArray (PrimArray ba) f

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

type instance SuperclassOf (JSByteArrayClass a) = 'Nothing

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

byteArrayByteLength :: (JSByteArrayElement a) => JSByteArray a -> Int
byteArrayByteLength = byteLength

byteArrayElementLength :: (JSByteArrayElement a) => JSByteArray a -> Int
byteArrayElementLength = elemLength

class (Storable a, Prim a) => JSByteArrayElement a where
  fromArrayBuffer_ :: ArrayBuffer -> IO (JSByteArray a)
  allocateArray :: Ptr a -> Int -> IO (JSByteArray a)
  byteLength :: JSByteArray a -> Int
  byteLength = js_buffer_byteLength
  byteOffset :: JSByteArray a -> Ptr a
  byteOffset = js_buffer_byteOffset
  elemLength :: JSByteArray a -> Int
  elemLength = js_buffer_elemLength

instance JSByteArrayElement Int8 where
  allocateArray = js_alloc_int8
  fromArrayBuffer_ = js_fromArrayBuffer__int8

instance JSByteArrayElement Int16 where
  allocateArray = js_alloc_int16
  fromArrayBuffer_ = js_fromArrayBuffer__int16

instance JSByteArrayElement Int32 where
  allocateArray = js_alloc_int32
  fromArrayBuffer_ = js_fromArrayBuffer__int32

instance JSByteArrayElement Int64 where
  allocateArray = js_alloc_bigint64
  fromArrayBuffer_ = js_fromArrayBuffer__bigint64

instance JSByteArrayElement Word8 where
  allocateArray = js_alloc_uint8
  fromArrayBuffer_ = js_fromArrayBuffer__uint8

instance JSByteArrayElement Word16 where
  allocateArray = js_alloc_uint16
  fromArrayBuffer_ = js_fromArrayBuffer__uint16

instance JSByteArrayElement Word32 where
  allocateArray = js_alloc_uint32
  fromArrayBuffer_ = js_fromArrayBuffer__uint32

instance JSByteArrayElement Word64 where
  allocateArray = js_alloc_biguint64
  fromArrayBuffer_ = js_fromArrayBuffer__biguint64

instance JSByteArrayElement Float where
  allocateArray = js_alloc_float32
  fromArrayBuffer_ = js_fromArrayBuffer__float32

instance JSByteArrayElement Double where
  allocateArray = js_alloc_float64
  fromArrayBuffer_ = js_fromArrayBuffer__float64

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

foreign import javascript unsafe "new Int8Array($1)"
  js_fromArrayBuffer__int8 :: ArrayBuffer -> IO Int8Array

foreign import javascript unsafe "new Uint8Array($1)"
  js_fromArrayBuffer__uint8 :: ArrayBuffer -> IO Uint8Array

foreign import javascript unsafe "new Int16Array($1)"
  js_fromArrayBuffer__int16 :: ArrayBuffer -> IO Int16Array

foreign import javascript unsafe "new Uint16Array($1)"
  js_fromArrayBuffer__uint16 :: ArrayBuffer -> IO Uint16Array

foreign import javascript unsafe "new Int32Array($1)"
  js_fromArrayBuffer__int32 :: ArrayBuffer -> IO Int32Array

foreign import javascript unsafe "new Uint32Array($1)"
  js_fromArrayBuffer__uint32 :: ArrayBuffer -> IO Uint32Array

foreign import javascript unsafe "new BigInt64Array($1)"
  js_fromArrayBuffer__bigint64 :: ArrayBuffer -> IO BigInt64Array

foreign import javascript unsafe "new BigUint64Array($1)"
  js_fromArrayBuffer__biguint64 :: ArrayBuffer -> IO BigUint64Array

foreign import javascript unsafe "new Float32Array($1)"
  js_fromArrayBuffer__float32 :: ArrayBuffer -> IO Float32Array

foreign import javascript unsafe "new Float64Array($1)"
  js_fromArrayBuffer__float64 :: ArrayBuffer -> IO Float64Array

foreign import javascript unsafe "$1.byteOffset"
  js_buffer_byteOffset :: JSByteArray a -> Ptr a

foreign import javascript unsafe "$1.byteLength"
  js_buffer_byteLength :: JSByteArray a -> Int

foreign import javascript unsafe "$1.length"
  js_buffer_elemLength :: JSByteArray a -> Int

type data DataViewClass :: Prototype

type instance SuperclassOf DataViewClass = 'Nothing

type DataView = JSObject DataViewClass

foreign import javascript unsafe "$1.getInt8($2)"
  getInt8 :: DataView -> Int -> IO Int8

foreign import javascript unsafe "$1.getUint8($2)"
  getUint8 :: DataView -> Int -> IO Word8

foreign import javascript unsafe "$1.getInt16($2)"
  getInt16 :: DataView -> Int -> IO Int16

foreign import javascript unsafe "$1.getUint16($2)"
  getUint16 :: DataView -> Int -> IO Word16

foreign import javascript unsafe "$1.getInt32($2)"
  getInt32 :: DataView -> Int -> IO Int32

foreign import javascript unsafe "$1.getUint32($2)"
  getUint32 :: DataView -> Int -> IO Word32

foreign import javascript unsafe "$1.getBigInt64($2)"
  getBigInt64 :: DataView -> Int -> IO Int64

foreign import javascript unsafe "$1.getBigUint64($2)"
  getBigUint64 :: DataView -> Int -> IO Word64

foreign import javascript unsafe "$1.getFloat32($2)"
  getFloat32 :: DataView -> Int -> IO Float

foreign import javascript unsafe "$1.getFloat64($2)"
  getFloat64 :: DataView -> Int -> IO Double

foreign import javascript unsafe "$1.setInt8($2, $3)"
  setInt8 :: DataView -> Int -> Int8 -> IO ()

foreign import javascript unsafe "$1.setUint8($2, $3)"
  setUint8 :: DataView -> Int -> Word8 -> IO ()

foreign import javascript unsafe "$1.setInt16($2, $3)"
  setInt16 :: DataView -> Int -> Int16 -> IO ()

foreign import javascript unsafe "$1.setUint16($2, $3)"
  setUint16 :: DataView -> Int -> Word16 -> IO ()

foreign import javascript unsafe "$1.setInt32($2, $3)"
  setInt32 :: DataView -> Int -> Int32 -> IO ()

foreign import javascript unsafe "$1.setUint32($2, $3)"
  setUint32 :: DataView -> Int -> Word32 -> IO ()

foreign import javascript unsafe "$1.setBigInt64($2, $3)"
  setBigInt64 :: DataView -> Int -> Int64 -> IO ()

foreign import javascript unsafe "$1.setBigUint64($2, $3)"
  setBigUint64 :: DataView -> Int -> Word64 -> IO ()

foreign import javascript unsafe "$1.setFloat32($2, $3)"
  setFloat32 :: DataView -> Int -> Float -> IO ()

foreign import javascript unsafe "$1.setFloat64($2, $3)"
  setFloat64 :: DataView -> Int -> Double -> IO ()

foreign import javascript unsafe "new DataView($1, $2)"
  toDataView :: ArrayBuffer -> Int -> IO DataView

foreign import javascript unsafe "$1.byteLength"
  dataViewByteLength :: DataView -> Int
