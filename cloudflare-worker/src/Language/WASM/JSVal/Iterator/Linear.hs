{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}

module Language.WASM.JSVal.Iterator.Linear (
  Iterator,
  next,
) where

import Data.Coerce (coerce)
import GHC.IO (unsafePerformIO)
import GHC.Wasm.Prim (JSVal, freeJSVal)
import Language.WASM.JSVal.Iterator qualified as NonLinear

data Iterator where
  Iterator :: NonLinear.Iterator -> Iterator

next :: Iterator %1 -> Maybe (JSVal, Iterator)
{-# NOINLINE next #-}
next (Iterator iter) = unsafePerformIO do
  NonLinear.pop iter >>= \case
    Nothing -> Nothing <$ freeJSVal (coerce iter)
    Just val -> pure $ Just (val, Iterator iter)
