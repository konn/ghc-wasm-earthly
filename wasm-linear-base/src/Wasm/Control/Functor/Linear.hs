{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Wasm.Control.Functor.Linear (
  Functor (..),
  (<$>),
  (<$),
  void,
  Applicative (..),
  Monad (..),
  return,
  join,
  MonadFail (..),
  ap,
  Data (..),
) where

import Data.Functor.Identity
import qualified Wasm.Data.Functor.Linear as Data
import Wasm.Data.Unrestricted.Linear
import Prelude (String)

class (Data.Functor f) => Functor f where
  fmap :: (a %1 -> b) %1 -> f a %1 -> f b

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)
  {-# INLINE fmap #-}

infixl 4 <$>, <$

(<$>) :: (Functor f) => (a %1 -> b) %1 -> f a %1 -> f b
(<$>) = fmap
{-# INLINE (<$>) #-}

(<$) :: (Functor f, Consumable b) => a %1 -> f b %1 -> f a
a <$ fb = fmap (`lseq` a) fb
{-# INLINE (<$) #-}

void :: (Functor f, Consumable a) => f a %1 -> f ()
void = fmap consume
{-# INLINE void #-}

class (Data.Applicative f, Functor f) => Applicative f where
  {-# MINIMAL pure, ((<*>), liftA2) #-}
  pure :: a %1 -> f a
  infixl 4 <*>
  (<*>) :: f (a %1 -> b) %1 -> f a %1 -> f b
  (<*>) = liftA2 (\g -> g)
  liftA2 :: (a %1 -> b %1 -> c) %1 -> f a %1 -> f b %1 -> f c
  liftA2 f x y = f <$> x <*> y

instance Applicative Identity where
  pure a = Identity a
  {-# INLINE pure #-}
  Identity f <*> Identity a = Identity (f a)
  {-# INLINE (<*>) #-}
  liftA2 f (Identity a) (Identity b) = Identity (f a b)
  {-# INLINE liftA2 #-}

class (Data.Applicative m, Applicative m) => Monad m where
  {-# MINIMAL (>>=) #-}
  infixl 1 >>=, >>
  (>>=) :: m a %1 -> (a %1 -> m b) %1 -> m b
  (>>) :: m () %1 -> m a %1 -> m a
  m >> k = m >>= (\() -> k)

return :: (Monad m) => a %1 -> m a
return = pure
{-# INLINE return #-}

join :: (Monad m) => m (m a) %1 -> m a
join m = m >>= (\x -> x)
{-# INLINE join #-}

ap :: (Monad m) => m (a %1 -> b) %1 -> m a %1 -> m b
ap f x = f >>= (\f' -> x >>= (\x' -> return (f' x')))
{-# INLINE ap #-}

class (Monad m) => MonadFail m where
  fail :: String -> m a

newtype Data f a = Data (f a)

instance (Functor f) => Data.Functor (Data f) where
  fmap f (Data x) = Data (fmap f x)
  {-# INLINE fmap #-}

instance (Applicative f) => Data.Applicative (Data f) where
  pure a = Data (pure a)
  {-# INLINE pure #-}
  Data f <*> Data x = Data (f <*> x)
  {-# INLINE (<*>) #-}
  liftA2 f (Data a) (Data b) = Data (liftA2 f a b)
  {-# INLINE liftA2 #-}
