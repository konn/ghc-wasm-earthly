{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module GHC.Wasm.Prim (
  -- User-facing JSVal type and freeJSVal
  JSVal (..),
  freeJSVal,
  -- The JSString type and conversion from/to Haskell String
  JSString (..),
  fromJSString,
  toJSString,
  -- Exception types related to JSFFI
  JSException (..),
  WouldBlockException (..),
  PromisePendingException (..),
  -- Is JSFFI used in the current wasm module?
  isJSFFIUsed,
) where

import Control.Exception (Exception)
import GHC.Exts
import GHC.IO
import GHC.Stable (StablePtr (..), freeStablePtr)
import GHC.Stack (HasCallStack)

newtype JSVal# = JSVal# (Any :: UnliftedType)

data JSVal = forall a. JSVal JSVal# (Weak# JSVal#) (StablePtr# a)

freeJSVal :: JSVal -> IO ()
freeJSVal (JSVal _ w sp) = do
  case sp `eqStablePtr#` unsafeCoerce# nullAddr# of
    0# -> do
      freeStablePtr $ StablePtr sp
    _ -> pure ()
  IO $ \s0 -> case finalizeWeak# w s0 of
    (# s1, _, _ #) -> (# s1, () #)

newtype JSString = JSString JSVal

fromJSString :: JSString -> String
fromJSString = error "fromJSString: not implemented"

toJSString :: String -> JSString
toJSString = error "toJSString: not implemented"

data JSException = JSException JSVal
  deriving anyclass (Exception)

instance Show JSException where
  showsPrec _ _ = showString "JSException"

data WouldBlockException
  = WouldBlockException
  deriving (Show)

instance Exception WouldBlockException

data PromisePendingException
  = PromisePendingException
  deriving (Show)

instance Exception PromisePendingException

isJSFFIUsed :: (HasCallStack) => Bool
isJSFFIUsed = error "isJSFFIUsed: not implemented"
