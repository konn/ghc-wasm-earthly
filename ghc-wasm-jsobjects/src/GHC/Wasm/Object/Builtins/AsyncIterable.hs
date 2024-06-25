{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnliftedDatatypes #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module GHC.Wasm.Object.Builtins.AsyncIterable (
  AsyncIterableClass,
  AsyncIterable,
  fromAsyncIterable,
  PairAsyncIterableClass,
  PairAsyncIterable,
  fromPairAsyncIterable,
) where

import Control.Arrow ((&&&))
import Data.Functor.Of (Of)
import GHC.Wasm.Object.Builtins.AsyncIterator
import GHC.Wasm.Object.Core
import qualified Streaming.Prelude as S

type data AsyncIterableClass :: Prototype -> Prototype

type instance SuperclassOf (AsyncIterableClass a) = 'Nothing

-- | Like 'Iterator', but explicit element type. See also: 'KVAsyncIterable'.
type AsyncIterable a = JSObject (AsyncIterableClass a)

fromAsyncIterable :: AsyncIterable a -> S.Stream (Of (JSObject a)) IO ()
fromAsyncIterable = S.map unsafeCast . fromAsyncIterator . unsafeCast

type data PairAsyncIterableClass :: Prototype -> Prototype -> Prototype

type instance SuperclassOf (PairAsyncIterableClass k v) = 'Nothing

-- | Like 'Iterator', but explicit element type. See also: 'KVPairAsyncIterable'.
type PairAsyncIterable k v = JSObject (PairAsyncIterableClass k v)

fromPairAsyncIterable :: PairAsyncIterable k v -> S.Stream (Of (JSObject k, JSObject k)) IO ()
fromPairAsyncIterable = S.map (js_fst &&& js_snd) . fromAsyncIterator . unsafeCast

foreign import javascript unsafe "$1[0]"
  js_fst :: JSAny -> JSObject k

foreign import javascript unsafe "$1[1]"
  js_snd :: JSAny -> JSObject k
