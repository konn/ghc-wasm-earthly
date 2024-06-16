{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnliftedDatatypes #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module GHC.Wasm.Object.Builtins.Sequence (
  -- * 'DOMString's
  Sequence,
  SequenceClass,
  fromSequence,
  toVector,
  toSequence,
) where

import Control.Arrow ((>>>))
import Data.Coerce (coerce)
import Data.Functor.Of (Of)
import qualified Data.Vector as V
import GHC.Wasm.Object.Builtins.Iterator (fromIterator, tryToIterator)
import GHC.Wasm.Object.Core
import qualified Streaming.Prelude as S
import qualified Wasm.Data.Array.Destination.JSVal as JSD

-- | A WebIDL @Sequence@, which corresponds to a JavaScript array of the same type.
type data SequenceClass :: Prototype -> Prototype

type instance SuperclassOf (SequenceClass a) = 'Nothing

type Sequence a = JSObject (SequenceClass a)

fromSequence :: Sequence a -> S.Stream (Of (JSObject a)) IO ()
fromSequence =
  tryToIterator >>> \case
    Right iter ->
      S.map coerce $ fromIterator iter
    -- This case cannot happen, if correctly typed.
    Left {} -> mempty

toVector :: Sequence a -> IO (V.Vector (JSObject a))
toVector = fromSequence >>> S.toList_ >>> fmap V.fromList

toSequence :: V.Vector (JSObject a) -> Sequence a
toSequence vec =
  coerce $ JSD.alloc (V.length vec) (JSD.mirror $ coerce vec)
