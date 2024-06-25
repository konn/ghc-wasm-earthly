{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnliftedDatatypes #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module GHC.Wasm.Object.Builtins.AsyncIterator (
  hasAsyncIterator,
  tryToAsyncIterator,
  fromAsyncIterator,
  JSAsyncIterator,
  JSAsyncIteratorClass,
) where

import Control.Monad (guard)
import Data.Functor.Of (Of)
import GHC.Wasm.Object.Builtins.Promise
import GHC.Wasm.Object.Core
import qualified Streaming.Prelude as S

hasAsyncIterator :: JSObject a -> Bool
hasAsyncIterator = js_is_AsyncIterator

data JSAsyncIteratorClass :: Prototype

type instance SuperclassOf JSAsyncIteratorClass = 'Nothing

type JSAsyncIterator = JSObject JSAsyncIteratorClass

fromAsyncIterator :: JSAsyncIterator -> S.Stream (Of JSAny) IO ()
fromAsyncIterator =
  S.reread \iter -> do
    resl <- await =<< js_AsyncIterator_next iter
    let val = js_AsyncIterator_value resl
    if js_AsyncIterator_done resl
      then pure $ val <$ guard (not $ js_is_undefined val)
      else pure $ Just val

data JSAsyncIteratorResultClass :: Prototype

type JSAsyncIteratorResult = JSObject JSAsyncIteratorResultClass

foreign import javascript unsafe "$1 === undefined"
  js_is_undefined :: JSAny -> Bool

foreign import javascript unsafe "$1.value"
  js_AsyncIterator_value :: JSAsyncIteratorResult -> JSAny

foreign import javascript unsafe "$1.done"
  js_AsyncIterator_done :: JSAsyncIteratorResult -> Bool

foreign import javascript safe "$1.next()"
  js_AsyncIterator_next :: JSAsyncIterator -> IO (Promise JSAsyncIteratorResultClass)

tryToAsyncIterator :: JSObject a -> Either (JSObject a) JSAsyncIterator
tryToAsyncIterator obj
  | hasAsyncIterator obj = Right (js_get_AsyncIterator obj)
  | otherwise = Left obj

foreign import javascript unsafe "if ($1[Symbol.asyncIterator]) { return true; } else { return false; }"
  js_is_AsyncIterator :: JSObject a -> Bool

foreign import javascript unsafe "$1[Symbol.AsyncIterator]()"
  js_get_AsyncIterator :: JSObject a -> JSAsyncIterator
