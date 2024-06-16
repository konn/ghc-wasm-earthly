{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnliftedDatatypes #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module GHC.Wasm.Object.Builtins.Iterator (
  hasIterator,
  tryToIterator,
  fromIterator,
  JSIterator,
  JSIteratorClass,
) where

import Control.Monad (guard)
import Data.Functor.Of (Of)
import GHC.Wasm.Object.Core
import qualified Streaming.Prelude as S

hasIterator :: JSObject a -> Bool
hasIterator = js_is_iterator

data JSIteratorClass :: Prototype

type instance SuperclassOf JSIteratorClass = 'Nothing

type JSIterator = JSObject JSIteratorClass

fromIterator :: JSIterator -> S.Stream (Of JSAny) IO ()
fromIterator =
  S.reread \iter -> do
    resl <- js_iterator_next iter
    let val = js_iterator_value resl
    if js_iterator_done resl
      then pure $ val <$ guard (js_is_undefined val)
      else pure $ Just val

data JSIteratorResultClass :: Prototype

type JSIteratorResult = JSObject JSIteratorResultClass

foreign import javascript unsafe "$1 === undefined"
  js_is_undefined :: JSAny -> Bool

foreign import javascript unsafe "$1.value"
  js_iterator_value :: JSIteratorResult -> JSAny

foreign import javascript unsafe "$1.done"
  js_iterator_done :: JSIteratorResult -> Bool

foreign import javascript unsafe "$1.next()"
  js_iterator_next :: JSIterator -> IO JSIteratorResult

tryToIterator :: JSObject a -> Either (JSObject a) JSIterator
tryToIterator obj
  | hasIterator obj = Right (js_get_iterator obj)
  | otherwise = Left obj

foreign import javascript unsafe "if ($1[Symbol.iterator]) { return true; } else { return false; }"
  js_is_iterator :: JSObject a -> Bool

foreign import javascript unsafe "$1[Symbol.iterator]()"
  js_get_iterator :: JSObject a -> JSIterator
