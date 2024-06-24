{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnliftedDatatypes #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module GHC.Wasm.Object.Builtins.Iterable (
  IterableClass,
  Iterable,
  fromIterable,
  PairIterableClass,
  PairIterable,
  fromPairIterable,
) where

import Control.Arrow ((&&&))
import Data.Functor.Of (Of)
import GHC.Wasm.Object.Builtins.Iterator
import GHC.Wasm.Object.Core
import qualified Streaming.Prelude as S

type data IterableClass :: Prototype -> Prototype

type instance SuperclassOf (IterableClass a) = 'Nothing

-- | Like 'Iterator', but explicit element type. See also: 'KVIterable'.
type Iterable a = JSObject (IterableClass a)

fromIterable :: Iterable a -> S.Stream (Of (JSObject a)) IO ()
fromIterable = S.map unsafeCast . fromIterator . unsafeCast

type data PairIterableClass :: Prototype -> Prototype -> Prototype

type instance SuperclassOf (PairIterableClass k v) = 'Nothing

-- | Like 'Iterator', but explicit element type. See also: 'KVPairIterable'.
type PairIterable k v = JSObject (PairIterableClass k v)

fromPairIterable :: PairIterable k v -> S.Stream (Of (JSObject k, JSObject k)) IO ()
fromPairIterable = S.map (js_fst &&& js_snd) . fromIterator . unsafeCast

foreign import javascript unsafe "$1[0]"
  js_fst :: JSAny -> JSObject k

foreign import javascript unsafe "$1[1]"
  js_snd :: JSAny -> JSObject k
