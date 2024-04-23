{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

module Data.Array.Destination.Micro (
  DArray (),
  withDArray,
  fill,
  dropEmpty,
  split,
) where

import Control.Exception (evaluate)
import Data.Vector qualified as V
import Data.Vector.Internal.Check (HasCallStack)
import Data.Vector.Mutable (RealWorld)
import Data.Vector.Mutable qualified as MV
import System.IO.Unsafe (unsafeDupablePerformIO)
import Unsafe.Coerce (unsafeCoerce)

data DArray a where
  DArray :: MV.MVector RealWorld a -> DArray a

dropEmpty :: (HasCallStack) => DArray a %1 -> ()
dropEmpty (DArray mv) =
  if MV.null mv
    then ()
    else error $ "dropEmpty: nonempty! got: " ++ show (MV.length mv)

fill :: (HasCallStack) => a %1 -> DArray a %1 -> ()
fill a (DArray mv) =
  case MV.length mv /= 1 of
    True -> error ("fill: non-one length; " <> show (MV.length mv)) a
    False -> unsafeLinear (\b -> unsafeDupablePerformIO $ MV.unsafeWrite mv 0 b) a

withDArray :: Int -> (DArray a %1 -> ()) %1 -> V.Vector a
{-# NOINLINE withDArray #-}
withDArray n = unsafeLinear \k -> unsafeDupablePerformIO do
  v <- MV.new n
  () <- evaluate $ k (DArray v)
  V.unsafeFreeze v

unsafeLinear :: (a -> b) %1 -> (a %1 -> b)
unsafeLinear = unsafeCoerce unsafeCoerce

split :: Int -> DArray a %1 -> (DArray a, DArray a)
split i (DArray mv)
  | (lh, rh) <- MV.splitAt i mv = (DArray lh, DArray rh)
