{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnliftedDatatypes #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module GHC.Wasm.Object.Builtins.Array (
  -- * Frozen Arrays
  FrozenArray,
  FrozenArrayClass,
  frozenToVector,
  lengthOfFrozenArray,
  indexFrozenArray,
  toFrozenArray,

  -- * Observable Arrays
  ObservableArrayClass,
  ObservableArray,
) where

import qualified Data.Vector as V
import GHC.Wasm.Object.Builtins.Sequence
import GHC.Wasm.Object.Core

type data FrozenArrayClass :: Prototype -> Prototype

type FrozenArray a = JSObject (ObservableArrayClass a)

type instance SuperclassOf (FrozenArrayClass a) = 'Nothing

frozenToVector :: FrozenArray a -> V.Vector (JSObject a)
frozenToVector arr = V.generate (lengthOfFrozenArray arr) (indexFrozenArray arr)

toFrozenArray :: V.Vector (JSObject a) -> IO (FrozenArray a)
toFrozenArray = fmap js_freeze_sequence . toSequence

foreign import javascript unsafe "$1.freeze(); $1"
  js_freeze_sequence :: Sequence a -> FrozenArray a

foreign import javascript unsafe "$1.length"
  lengthOfFrozenArray :: FrozenArray a -> Int

foreign import javascript unsafe "$1[$2]"
  indexFrozenArray :: FrozenArray a -> Int -> JSObject a

type instance SuperclassOf (FrozenArrayClass a) = 'Nothing

-- TODO: Implement the 'ObservableArray' API
type data ObservableArrayClass :: Prototype -> Prototype

type ObservableArray a = JSObject (ObservableArrayClass a)

type instance SuperclassOf (ObservableArrayClass a) = 'Nothing
