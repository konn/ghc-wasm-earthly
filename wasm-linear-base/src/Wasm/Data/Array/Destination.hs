{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

module Wasm.Data.Array.Destination (
  DArray (),
  withDArray,
  withDArrayM,
  fill,
  dropEmpty,
  split,
) where

import Control.Exception (evaluate)
import qualified Data.Vector as V
import Data.Vector.Internal.Check (HasCallStack)
import Data.Vector.Mutable (RealWorld)
import qualified Data.Vector.Mutable as MV
import System.IO.Unsafe (unsafeDupablePerformIO)
import qualified Wasm.Control.Functor.Linear as Control
import Wasm.Data.Unrestricted.Linear (Ur (..))
import qualified Wasm.System.IO.Linear as LIO
import qualified Wasm.Unsafe.Linear as Unsafe

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
    False -> Unsafe.toLinear (\b -> unsafeDupablePerformIO $ MV.unsafeWrite mv 0 b) a

withDArrayM :: Int -> (DArray a %1 -> LIO.IO ()) %1 -> LIO.IO (V.Vector a)
{-# NOINLINE withDArrayM #-}
withDArrayM n k = Control.do
  Ur v <- LIO.fromSystemIOU (MV.new n)
  () <- k (DArray v)
  LIO.fromSystemIO (V.unsafeFreeze v)

withDArray :: Int -> (DArray a %1 -> ()) %1 -> V.Vector a
{-# NOINLINE withDArray #-}
withDArray n = Unsafe.toLinear \k -> unsafeDupablePerformIO do
  v <- MV.new n
  () <- evaluate $ k (DArray v)
  V.unsafeFreeze v

split :: Int -> DArray a %1 -> (DArray a, DArray a)
split i (DArray mv)
  | (lh, rh) <- MV.splitAt i mv = (DArray lh, DArray rh)
