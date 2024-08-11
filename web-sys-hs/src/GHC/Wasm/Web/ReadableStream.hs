{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnliftedDatatypes #-}

module GHC.Wasm.Web.ReadableStream (
  -- * Construction
  toReadableStream,
  fromLazyByteString,

  -- ** Custom streams
  newPushReadableStream,
  StartRawStream,
  newPullReadableStream,
  PullRawStream,
  CancelRawStream,

  -- *** Operations on stream controllers
  StreamController,
  enqueueBS,
  closeStream,

  -- * Consumption
  fromReadableStream,

  -- ** Low-level combinators
  ReadableStreamDefaultReader,
  ReadableStreamDefaultReaderClass,
  getReader,
  popReader,
  module GHC.Wasm.Web.Generated.ReadableStream,
) where

import Control.Exception (finally)
import Control.Monad ((<=<))
import Control.Monad.Trans.Class (lift)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Internal as LBS
import Data.IORef
import Data.Kind (Type)
import Data.Word
import Foreign.StablePtr
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.ReadableStream
import GHC.Wasm.Web.Generated.ReadableStreamDefaultReader
import qualified Streaming.ByteString as Q

fromLazyByteString :: LBS.LazyByteString -> IO ReadableStream
fromLazyByteString lbs0 = do
  ref <- newIORef lbs0
  newPullReadableStream pull mempty ref
  where
    pull ref ctrl = do
      lbs <- readIORef ref
      case lbs of
        LBS.Empty -> closeStream ctrl
        LBS.Chunk s lo -> do
          enqueueBS s ctrl
          writeIORef ref lo

toReadableStream :: Q.ByteStream IO () -> IO ReadableStream
toReadableStream =
  newPushReadableStream start stop
  where
    start q ctrl = do
      Q.chunkMapM_ (flip enqueueBS ctrl) q
        `finally` closeStream ctrl
    stop _ _ = pure ()

closeStream :: StreamController -> IO ()
closeStream = js_cls

enqueueBS :: BS.ByteString -> StreamController -> IO ()
enqueueBS bs = useByteStringAsJSByteArray bs . js_enq

newPushReadableStream ::
  StartStream a ->
  CancelStream a ->
  a ->
  IO ReadableStream
newPushReadableStream start cancel a = do
  ref <- newStablePtr a
  start' <- js_mk_StrartRawStream \sptr ctrl -> do
    x <- deRefStablePtr sptr
    start x ctrl
  cancel' <- js_mk_CancelRawStream \sptr ctrl ->
    do { x <- deRefStablePtr sptr; cancel x ctrl } `finally` freeStablePtr sptr
  js_new_CustomReadableStream_push ref start' cancel'

newPullReadableStream :: PullStream a -> CancelStream a -> a -> IO ReadableStream
newPullReadableStream pull cancel a = do
  ref <- newStablePtr a
  pull' <- js_mk_PullRawStream \sptr ctrl ->
    do { x <- deRefStablePtr sptr; pull x ctrl } `finally` freeStablePtr sptr
  cancel' <- js_mk_CancelRawStream \sptr ctrl -> do
    do { x <- deRefStablePtr sptr; cancel x ctrl } `finally` freeStablePtr sptr
  js_new_CustomReadableStream_pull ref pull' cancel'

type data StreamControllerClass :: Prototype

type instance SuperclassOf StreamControllerClass = 'Nothing

type StreamController = JSObject StreamControllerClass

type data JSStartStreamClass :: Prototype

type instance SuperclassOf JSStartStreamClass = 'Nothing

type data JSStartRawStreamClass :: Type -> Prototype

type instance SuperclassOf (JSStartRawStreamClass a) = 'Nothing

type JSStartRawStream a = JSObject (JSStartRawStreamClass a)

type data JSPullRawStreamClass :: Type -> Prototype

type instance SuperclassOf (JSPullRawStreamClass a) = 'Nothing

type JSPullRawStream a = JSObject (JSPullRawStreamClass a)

type data JSCancelStreamClass :: Prototype

type instance SuperclassOf JSCancelStreamClass = 'Nothing

type data JSCancelRawStreamClass :: Type -> Prototype

type instance SuperclassOf (JSCancelRawStreamClass a) = 'Nothing

type JSCancelRawStream a = JSObject (JSCancelRawStreamClass a)

type StartRawStream a = StablePtr a -> StreamController -> IO ()

type PullRawStream a = StablePtr a -> StreamController -> IO ()

type CancelRawStream a = StablePtr a -> USVString -> IO ()

type StartStream a = a -> StreamController -> IO ()

type PullStream a = a -> StreamController -> IO ()

type CancelStream a = a -> USVString -> IO ()

foreign import javascript unsafe "$1.enqueue($2)"
  js_enq :: StreamController -> Uint8Array -> IO ()

foreign import javascript unsafe "$1.close()"
  js_cls :: StreamController -> IO ()

foreign import javascript unsafe "wrapper"
  js_mk_StrartRawStream :: StartRawStream a -> IO (JSStartRawStream a)

foreign import javascript unsafe "wrapper"
  js_mk_PullRawStream :: PullRawStream a -> IO (JSPullRawStream a)

foreign import javascript unsafe "wrapper"
  js_mk_CancelRawStream :: CancelRawStream a -> IO (JSCancelRawStream a)

foreign import javascript unsafe "new (class _ extends ReadableStream {\
  \constructor() {\
    \super({\
      \start: (ctrl) => { $2(this.__ref, ctrl) }, \
      \cancel: (ctrl) => { $3(this.__ref, ctrl) }, \
      \type: 'bytes' \
    \});\
    \this.__ref = $1;\
  \}})"
  js_new_CustomReadableStream_push ::
    StablePtr a ->
    JSStartRawStream a ->
    JSCancelRawStream a ->
    IO ReadableStream

foreign import javascript unsafe "new (class _ extends ReadableStream {\
  \constructor() {\
    \super({\
      \pull: (ctrl) => { $2(this.__ref, ctrl) }, \
      \cancel: (ctrl) => { $3(this.__ref, ctrl) }, \
      \type: 'bytes' \
    \});\
    \this.__ref = $1;\
  \}})"
  js_new_CustomReadableStream_pull ::
    StablePtr a ->
    JSPullRawStream a ->
    JSCancelRawStream a ->
    IO ReadableStream

fromReadableStream :: ReadableStream -> Q.ByteStream IO ()
fromReadableStream =
  Q.reread popReader
    <=< lift . getReader

getReader :: ReadableStream -> IO ReadableStreamDefaultReader
getReader =
  fmap (unsafeCast @_ @(ReadableStreamDefaultReaderClass))
    . flip
      js_fun_getReader_nullable_ReadableStreamGetReaderOptions_ReadableStreamReader
      none

popReader :: ReadableStreamDefaultReader -> IO (Maybe BS.ByteString)
popReader inp = do
  resl <- await =<< js_fun_read__Promise_ReadableStreamReadResult inp
  done <- getDictField "done" resl
  value <-
    fmap (toByteString . unsafeCast @_ @(JSByteArrayClass Word8)) . fromNullable
      <$> getDictField "value" resl
  if maybe True fromJSPrim $ fromNullable done
    then pure Nothing
    else pure value
