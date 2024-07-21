{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnliftedDatatypes #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Network.Cloudflare.Worker.Binding.R2 (
  R2,
  R2Class,

  -- * Methods
  head,
  get,
  getWith,
  RawGetOptions,
  GetOptionsClass,

  -- * Object Metadata
  R2Object,
  getObjectKey,
  getObjectVersion,
  getObjectSize,
  getObjectETagRaw,
  getObjectHTTPETag,
  getObjectCustomMetadata,
  getObjectStorageClass,
  writeObjectHttpMetadata,

  -- * Object Body
  R2ObjectBody,
  getBody,
  isBodyUsed,
  getBodyArrayBuffer,
  getBodyText,
  getBodyBlob,

  -- * Http Metadata
  R2HTTPMetadata,
  R2HTTPMetadataClass,
  R2HTTPMetadataFields,

  -- * Ranged read data
  R2RangeClass,
  R2OffsetRangeClass,
  R2LengthRangeClass,
  R2SuffixRangeClass,

  -- * Conditionals
  R2ConditionalClass,
  RawR2Conditional,

  -- * Checksums
  R2Checksums,
  R2ChecksumsClass,
  R2ChecksumsFields,
) where

import Control.Concurrent.Async (Async)
import Control.Monad ((<=<))
import Data.ByteString qualified as BS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text.Encoding qualified as TE
import Data.Word
import GHC.Generics (Generic (..))
import GHC.IO.Unsafe (unsafeDupablePerformIO)
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.Blob (BlobClass)
import GHC.Wasm.Web.Generated.Headers (Headers, HeadersClass)
import GHC.Wasm.Web.ReadableStream
import Lens.Family.Total
import Prelude hiding (all, head)

----------------
-- Core types
----------------

type data R2Class :: Prototype

type instance SuperclassOf R2Class = 'Nothing

type R2 = JSObject R2Class

type data R2ObjectBodyClass :: Prototype

type instance SuperclassOf R2ObjectBodyClass = 'Just R2ObjectClass

type R2ObjectBody = JSObject R2ObjectBodyClass

type data R2ObjectClass :: Prototype

type instance SuperclassOf R2ObjectClass = 'Nothing

type R2Object = JSObject R2ObjectClass

type R2HTTPMetadataFields =
  '[ '("contentType", NullableClass JSByteStringClass)
   , '("contentLanguage", NullableClass JSByteStringClass)
   , '("contentDisposition", NullableClass JSByteStringClass)
   , '("contentEncoding", NullableClass JSByteStringClass)
   , '("cacheControl", NullableClass JSByteStringClass)
   , '("cacheExpiry", NullableClass JSByteStringClass)
   ]

type R2HTTPMetadataClass = JSDictionaryClass R2HTTPMetadataFields

type R2HTTPMetadata = JSObject R2HTTPMetadataClass

type R2ChecksumsFields =
  '[ '("md5", NullableClass ArrayBufferClass)
   , '("sha1", NullableClass ArrayBufferClass)
   , '("sha256", NullableClass ArrayBufferClass)
   , '("sha384", NullableClass ArrayBufferClass)
   , '("sha512", NullableClass ArrayBufferClass)
   ]

type R2ChecksumsClass = JSDictionaryClass R2ChecksumsFields

type R2Checksums = JSObject R2ChecksumsClass

type R2RangeClass = UnionClass '[R2OffsetRangeClass, R2LengthRangeClass, R2SuffixRangeClass]

type R2OffsetRangeClass =
  JSDictionaryClass
    '[ '("offset", JSPrimClass Word64)
     , '("length", NullableClass (JSPrimClass Word64))
     ]

type R2LengthRangeClass =
  JSDictionaryClass
    '[ '("offset", NullableClass (JSPrimClass Word64))
     , '("length", JSPrimClass Word64)
     ]

type R2SuffixRangeClass =
  JSDictionaryClass
    '[ '("suffix", JSPrimClass Word64)]

type R2ObjectFields =
  '[ '("key", JSByteStringClass)
   , '("version", JSByteStringClass)
   , '("size", JSPrimClass Word64)
   , '("etag", JSByteStringClass)
   , '("httpEtag", JSByteStringClass)
   , '("uploaded", DateClass)
   , '("httpMetadata", R2HTTPMetadataClass)
   , '("customMetadata", JSRecordClass JSByteStringClass JSByteStringClass)
   , '("range", R2RangeClass)
   , '("checksums", R2ChecksumsClass)
   , '("storageClass", EnumClass '["Standard", "InfrequentAccess"])
   ]

asR2ObjFields :: (r2obj <: R2ObjectClass) => JSObject r2obj -> JSDictionary R2ObjectFields
asR2ObjFields = unsafeCast

getObjectKey :: (r2Obj <: R2ObjectClass) => JSObject r2Obj -> BS.ByteString
getObjectKey = unsafeDupablePerformIO . (toHaskellByteString <=< getDictField "key" . asR2ObjFields)

getObjectVersion :: (r2Obj <: R2ObjectClass) => JSObject r2Obj -> BS.ByteString
getObjectVersion = unsafeDupablePerformIO . (toHaskellByteString <=< getDictField "version" . asR2ObjFields)

getObjectSize :: (r2Obj <: R2ObjectClass) => JSObject r2Obj -> Word64
getObjectSize = fromJSPrim . unsafeDupablePerformIO . getDictField "size" . asR2ObjFields

getObjectETagRaw :: (r2Obj <: R2ObjectClass) => JSObject r2Obj -> BS.ByteString
getObjectETagRaw = unsafeDupablePerformIO . (toHaskellByteString <=< getDictField "etag" . asR2ObjFields)

getObjectHTTPETag :: (r2Obj <: R2ObjectClass) => JSObject r2Obj -> BS.ByteString
getObjectHTTPETag = unsafeDupablePerformIO . (toHaskellByteString <=< getDictField "httpEtag" . asR2ObjFields)

writeObjectHttpMetadata :: (r2Obj <: R2ObjectClass) => JSObject r2Obj -> Headers -> IO ()
writeObjectHttpMetadata = js_ffi_write_obj_md . upcast

getObjectCustomMetadata :: (r2Obj <: R2ObjectClass) => JSObject r2Obj -> Map BS.ByteString BS.ByteString
getObjectCustomMetadata =
  Map.mapKeys TE.encodeUtf8
    . unsafeDupablePerformIO
    . ( mapM toHaskellByteString
          <=< fromJSRecord
          <=< getDictField "customMetadata" . asR2ObjFields
      )

getObjectStorageClass :: (r2Obj <: R2ObjectClass) => JSObject r2Obj -> StorageClass
getObjectStorageClass = asStorageClass . unsafeDupablePerformIO . getDictField "storageClass" . asR2ObjFields

asStorageClass :: JSEnum '["Standard", "InfrequentAccess"] -> StorageClass
asStorageClass =
  _case
    & onEnum #"Standard" Standard
    & onEnum #"InfrequentAccess" InfrequentAccess

data StorageClass = Standard | InfrequentAccess
  deriving stock (Eq, Show, Generic)

head :: R2 -> BS.ByteString -> IO (Async (Maybe R2Object))
head r2 key = deferWith fromNullable =<< js_head r2 =<< fromHaskellByteString key

get :: R2 -> BS.ByteString -> IO (Async (Maybe R2ObjectBody))
get r2 key = deferWith fromNullable =<< js_get r2 =<< fromHaskellByteString key

getWith ::
  R2 ->
  BS.ByteString ->
  RawGetOptions ->
  IO (Async (Maybe (Either R2Object R2ObjectBody)))
getWith r2 key opts = do
  key' <- fromHaskellByteString key
  deferWith (fmap decBody . fromNullable) =<< js_get' r2 key' opts

decBody :: Union [R2ObjectClass, R2ObjectBodyClass] -> Either R2Object R2ObjectBody
decBody obj =
  if js_has_body obj
    then Right $ unsafeCast obj
    else Left $ unsafeCast obj

type GetOptionsClass =
  JSDictionaryClass
    '[ '("onlyIf", NullableClass (UnionClass '[R2ConditionalClass, HeadersClass]))
     , '("range", NullableClass R2RangeClass)
     ]

type RawGetOptions = JSObject GetOptionsClass

type R2ConditionalClass =
  JSDictionaryClass
    '[ '("etagMatches", NullableClass JSByteStringClass)
     , '("etagDoesNotMatch", NullableClass JSByteStringClass)
     , '("uploadedBefore", NullableClass DateClass)
     , '("uploadedAfter", NullableClass DateClass)
     ]

type RawR2Conditional = JSObject R2ConditionalClass

foreign import javascript unsafe "$1.body"
  getBody :: R2ObjectBody -> IO ReadableStream

foreign import javascript unsafe "$1.bodyUsed"
  isBodyUsed :: R2ObjectBody -> IO Bool

foreign import javascript safe "$1.arrayBuffer()"
  getBodyArrayBuffer :: R2ObjectBody -> IO (Promise ArrayBufferClass)

foreign import javascript safe "$1.text()"
  getBodyText :: R2ObjectBody -> IO (Promise USVStringClass)

foreign import javascript safe "$1.blob()"
  getBodyBlob :: R2ObjectBody -> IO (Promise BlobClass)

foreign import javascript safe "$1.blob()"
  js_ffi_write_obj_md :: R2Object -> Headers -> IO ()

foreign import javascript safe "$1.head($2)"
  js_head :: R2 -> JSByteString -> IO (Promise (NullableClass R2ObjectClass))

foreign import javascript safe "$1.get($2)"
  js_get ::
    R2 ->
    JSByteString ->
    IO (Promise (NullableClass R2ObjectBodyClass))

foreign import javascript safe "$1.get($2, $3)"
  js_get' ::
    R2 ->
    JSByteString ->
    RawGetOptions ->
    IO (Promise (NullableClass (UnionClass '[R2ObjectClass, R2ObjectBodyClass])))

foreign import javascript unsafe "$1.hasOwnProperty('body')"
  js_has_body :: Union '[R2ObjectClass, R2ObjectBodyClass] -> Bool
