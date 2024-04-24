{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Wasm.Data.Functor.Linear (
  Functor (..),
  (<$>),
  (<$),
  void,
  Applicative (..),
) where

import qualified Data.Coerce as DC
import Data.Either
import Data.Functor.Compose
import Data.Functor.Identity
import Data.Functor.Product
import Data.Maybe
import Wasm.Data.Unrestricted.Linear
import qualified Wasm.Unsafe.Linear as Unsafe
import qualified Prelude

class Functor f where
  fmap :: (a %1 -> b) -> f a %1 -> f b

instance Functor Identity where
  fmap = DC.coerce
  {-# INLINE fmap #-}

instance Functor Ur where
  fmap f (Ur a) = Ur (f a)
  {-# INLINE fmap #-}

instance Functor Maybe where
  fmap _ = Unsafe.coerce (Prelude.fmap @Maybe)
  {-# INLINE fmap #-}

instance Functor [] where
  fmap = Unsafe.coerce Prelude.map
  {-# INLINE fmap #-}

instance Functor (Either e) where
  fmap = Unsafe.coerce (Prelude.fmap @(Either e))
  {-# INLINE fmap #-}

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose x) = Compose (fmap (fmap f) x)
  {-# INLINE fmap #-}

instance (Functor f, Functor g) => Functor (Product f g) where
  fmap f (Pair x y) = Pair (fmap f x) (fmap f y)
  {-# INLINE fmap #-}

infixl 4 <$>, <$

(<$>) :: (Functor f) => (a %1 -> b) -> f a %1 -> f b
(<$>) = fmap

(<$) :: (Functor f, Consumable b) => a -> f b %1 -> f a
a <$ fb = fmap (`lseq` a) fb

void :: (Functor f, Consumable a) => f a %1 -> f ()
void = fmap consume
{-# INLINE void #-}

class (Functor f) => Applicative f where
  {-# MINIMAL pure, ((<*>) | liftA2) #-}
  pure :: a -> f a
  infixl 4 <*>
  (<*>) :: f (a %1 -> b) %1 -> f a %1 -> f b
  (<*>) = liftA2 (\g -> g)
  liftA2 :: (a %1 -> b %1 -> c) -> f a %1 -> f b %1 -> f c
  liftA2 f x y = f <$> x <*> y

instance Applicative Identity where
  pure = DC.coerce
  {-# INLINE pure #-}
  liftA2 f (Identity a) (Identity b) = Identity (f a b)
  {-# INLINE liftA2 #-}
  Identity f <*> Identity x = Identity (f x)
  {-# INLINE (<*>) #-}

instance Applicative Ur where
  pure = Ur
  {-# INLINE pure #-}
  liftA2 f (Ur a) (Ur b) = Ur (f a b)
  {-# INLINE liftA2 #-}
  Ur f <*> Ur x = Ur (f x)
  {-# INLINE (<*>) #-}

instance (Applicative f, Applicative g) => Applicative (Product f g) where
  pure a = Pair (pure a) (pure a)
  {-# INLINE pure #-}
  Pair f g <*> Pair x y = Pair (f <*> x) (g <*> y)
  {-# INLINE (<*>) #-}
  liftA2 h (Pair a b) (Pair x y) = Pair (liftA2 h a x) (liftA2 h b y)
  {-# INLINE liftA2 #-}

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure a = Compose (pure (pure a))
  {-# INLINE pure #-}
  Compose f <*> Compose x = Compose (liftA2 (<*>) f x)
  {-# INLINE (<*>) #-}
  liftA2 h (Compose a) (Compose b) = Compose (liftA2 (liftA2 h) a b)
  {-# INLINE liftA2 #-}
