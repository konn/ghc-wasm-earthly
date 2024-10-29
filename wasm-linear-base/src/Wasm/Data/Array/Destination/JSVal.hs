{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Wasm.Data.Array.Destination.JSVal (
  JSArray (..),
  alloc,
  allocIO,
  DJSArray (),
  replicate,
  size,
  fill,
  dropEmpty,
  splitAt,
  fromFunction,
  mirror,
) where

import Data.Ord (Ord (..))
import qualified Data.Vector as V
import GHC.Base (unIO)
import GHC.Exts (runRW#)
import GHC.IO (unsafeDupablePerformIO)
import GHC.Stack
import GHC.Wasm.Prim
import qualified System.IO as System
import qualified Wasm.Control.Functor.Linear as Control
import Wasm.Prelude.Linear (Ur (Ur), lseq, ($))
import qualified Wasm.Prelude.Linear as PL
import qualified Wasm.System.IO.Linear as LIO
import qualified Wasm.Unsafe.Linear as Unsafe
import Prelude (Eq (..), Int, Num (..), error, otherwise, pure)
import qualified Prelude as P

newtype JSArray = JSArray JSVal

data DJSArray where
  -- | Offset, length, body
  DJSArray :: !Int -> !Int -> JSArray -> DJSArray

foreign import javascript unsafe "Array($1)"
  js_new_array :: Int -> System.IO JSArray

foreign import javascript unsafe "$3[$1] = $2"
  js_set_array :: Int -> JSVal -> JSArray %1 -> System.IO ()

replicate :: JSVal -> DJSArray %1 -> ()
{-# NOINLINE replicate #-}
replicate !v (DJSArray off len arr) = go 0
  where
    go !i
      | i < len =
          case runRW# (unIO $ js_set_array (off + i) v arr) of
            (# _, () #) -> go (i + 1)
      | otherwise = ()

alloc :: Int -> (DJSArray %1 -> ()) %1 -> JSArray
{-# NOINLINE alloc #-}
alloc len f = (\(Ur dest) -> f (DJSArray 0 len dest) `lseq` dest)
  PL.$ unsafeDupablePerformIO do
    destArr <- js_new_array len
    pure $ Ur destArr

allocIO :: Int -> (DJSArray %1 -> LIO.IO ()) %1 -> LIO.IO JSArray
allocIO len f = Control.do
  Ur destArr <- LIO.fromSystemIOU PL.$ js_new_array len
  f (DJSArray 0 len destArr)
  Control.pure destArr

-- | Get the size of a destination array.
size :: DJSArray %1 -> (Ur Int, DJSArray)
size (DJSArray off len mvec) = (Ur len, DJSArray off len mvec)

fill :: (HasCallStack) => JSVal %1 -> DJSArray %1 -> ()
fill a (DJSArray off len mvec) =
  if len /= 1
    then error "Destination.JSVal.fill: requires a destination of size 1" $ a
    else
      a
        PL.& Unsafe.toLinear (\x -> js_set_array off x mvec `P.seq` ())

fromFunction :: (Int -> JSVal) -> DJSArray %1 -> ()
{-# NOINLINE fromFunction #-}
fromFunction f (DJSArray off len mvec) = go 0
  where
    go !i
      | i < len =
          case runRW# (unIO (js_set_array (off + i) (f i) mvec)) of
            (# !_, () #) -> go (i + 1)
      | otherwise = ()

mirror :: (HasCallStack) => V.Vector JSVal -> DJSArray %1 -> ()
mirror v arr =
  size arr PL.& \(Ur sz, arr) ->
    if V.length v < sz
      then error "Destination.JSVal.mirror: source too short" arr
      else fromFunction (v V.!) arr

dropEmpty :: (HasCallStack) => DJSArray %1 -> ()
dropEmpty (DJSArray _ len (JSArray v)) =
  if len == 0
    then ()
    else error "Destination.JSVal.dropEmpty: nonempty! got: " v

splitAt :: Int -> DJSArray %1 -> (DJSArray, DJSArray)
splitAt i (DJSArray off len mvec) =
  (DJSArray off i mvec, DJSArray (P.min off (off + i)) (P.max 0 (len - i)) mvec)
{-# INLINE splitAt #-}
