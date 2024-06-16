{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

module Wasm.Data.Array.Destination.Generic (
  DArray (),
  withDArray,
  withDArrayM,
  fill,
  dropEmpty,
  split,
) where

import Control.Exception (evaluate)
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as MG
import Data.Vector.Internal.Check (HasCallStack)
import Data.Vector.Mutable (RealWorld)
import System.IO.Unsafe (unsafeDupablePerformIO)
import qualified Wasm.Control.Functor.Linear as Control
import Wasm.Data.Unrestricted.Linear (Ur (..))
import qualified Wasm.System.IO.Linear as LIO
import qualified Wasm.Unsafe.Linear as Unsafe

data DArray v a where
  DArray :: G.Mutable v RealWorld a -> DArray v a

dropEmpty :: (HasCallStack, G.Vector v a) => DArray v a %1 -> ()
dropEmpty (DArray mv) =
  if MG.null mv
    then ()
    else error $ "dropEmpty: nonempty! got: " ++ show (MG.length mv)

fill :: (HasCallStack, G.Vector v a) => a %1 -> DArray v a %1 -> ()
fill a (DArray mv) =
  if MG.length mv /= 1
    then error ("fill: non-one length; " <> show (MG.length mv)) a
    else Unsafe.toLinear (\b -> unsafeDupablePerformIO $ MG.unsafeWrite mv 0 b) a

withDArrayM :: (G.Vector v a) => Int -> (DArray v a %1 -> LIO.IO ()) %1 -> LIO.IO (v a)
{-# NOINLINE withDArrayM #-}
withDArrayM n k = Control.do
  Ur v <- LIO.fromSystemIOU (MG.new n)
  () <- k (DArray v)
  LIO.fromSystemIO (G.unsafeFreeze v)

withDArray :: (G.Vector v a) => Int -> (DArray v a %1 -> ()) %1 -> v a
{-# NOINLINE withDArray #-}
withDArray n = Unsafe.toLinear \k -> unsafeDupablePerformIO do
  v <- MG.new n
  () <- evaluate $ k (DArray v)
  G.unsafeFreeze v

split :: (G.Vector v a) => Int -> DArray v a %1 -> (DArray v a, DArray v a)
split i (DArray mv)
  | (lh, rh) <- MG.splitAt i mv = (DArray lh, DArray rh)
