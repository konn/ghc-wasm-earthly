{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Wasm.System.IO.Linear (
  IO (..),
  fromSystemIO,
  fromSystemIOU,
  toSystemIO,
  withLinearIO,
  throwIO,
  catch,
  mask_,
  MonadIO (..),
) where

import Control.Exception (Exception)
import qualified Control.Exception as System
import GHC.Base (RealWorld, State#)
import qualified GHC.Base as System
import qualified Wasm.Control.Functor.Linear as Control
import qualified Wasm.Data.Functor.Linear as Data
import Wasm.Data.Unrestricted.Linear
import qualified Wasm.Unsafe.Linear as Unsafe
import qualified Prelude

newtype IO a = IO (State# RealWorld %1 -> (# State# RealWorld, a #))

type role IO representational

fromSystemIO :: System.IO a %1 -> IO a
fromSystemIO = Unsafe.coerce

fromSystemIOU :: System.IO a -> IO (Ur a)
fromSystemIOU action =
  fromSystemIO (Ur Prelude.<$> action)

toSystemIO :: IO a %1 -> System.IO a
toSystemIO (IO f) = System.IO (\s -> f s)

withLinearIO :: IO (Ur a) -> System.IO a
withLinearIO action = (\x -> unur x) Prelude.<$> (toSystemIO action)

throwIO :: (Exception e) => e -> IO a
throwIO e = fromSystemIO (System.throwIO e)

catch :: (Exception e) => IO (Ur a) -> (e -> IO (Ur a)) -> IO (Ur a)
catch action handler = fromSystemIO (System.catch (toSystemIO action) (\e -> toSystemIO (handler e)))

mask_ :: IO a -> IO a
mask_ action = fromSystemIO (System.mask_ (toSystemIO action))

instance Data.Functor IO where
  fmap f (IO g) = IO (\s -> case g s of (# s', a #) -> (# s', f a #))
  {-# INLINE fmap #-}

instance Data.Applicative IO where
  pure a = IO (\s -> (# s, a #))
  {-# INLINE pure #-}
  IO f <*> IO g = IO (\s -> case f s of (# s', h #) -> case g s' of (# s'', a #) -> (# s'', h a #))
  {-# INLINE (<*>) #-}
  liftA2 f (IO g) (IO h) = IO (\s -> case g s of (# s', a #) -> case h s' of (# s'', b #) -> (# s'', f a b #))
  {-# INLINE liftA2 #-}

instance Control.Functor IO where
  fmap f (IO g) = IO (\s -> case g s of (# s', a #) -> (# s', f a #))
  {-# INLINE fmap #-}

instance Control.Applicative IO where
  pure a = IO (\s -> (# s, a #))
  {-# INLINE pure #-}
  IO f <*> IO g = IO (\s -> case f s of (# s', h #) -> case g s' of (# s'', a #) -> (# s'', h a #))
  {-# INLINE (<*>) #-}
  liftA2 f (IO g) (IO h) = IO (\s -> case g s of (# s', a #) -> case h s' of (# s'', b #) -> (# s'', f a b #))
  {-# INLINE liftA2 #-}

class (Control.Monad m) => MonadIO m where
  liftIO :: IO a %1 -> m a
  liftSystemIO :: System.IO a -> m a
  liftSystemIO io = liftIO (fromSystemIO io)
  liftSystemIOU :: System.IO a -> m (Ur a)
  liftSystemIOU io = liftIO (fromSystemIOU io)
