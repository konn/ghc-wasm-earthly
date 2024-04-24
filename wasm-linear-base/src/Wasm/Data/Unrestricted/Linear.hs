{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}

module Wasm.Data.Unrestricted.Linear (
  Ur (..),
  unur,
  lift,
  lift2,
  Consumable (..),
  Dupable (..),
  Movable (..),
  lseq,
  dup,
  AsMovable (..),
) where

import qualified Data.Monoid as Mon
import GHC.Int
import GHC.Word
import qualified Wasm.Unsafe.Linear as Unsafe

data Ur a where Ur :: a -> Ur a

instance Functor Ur where
  fmap f (Ur a) = Ur (f a)

instance Applicative Ur where
  pure = Ur
  Ur f <*> Ur a = Ur (f a)
  liftA2 f (Ur a) (Ur b) = Ur (f a b)

unur :: Ur a %1 -> a
unur (Ur a) = a

lift :: (a -> b) -> Ur a %1 -> Ur b
lift f (Ur a) = Ur (f a)

lift2 :: (a -> b -> c) -> Ur a %1 -> Ur b %1 -> Ur c
lift2 f (Ur a) (Ur b) = Ur (f a b)

class Consumable a where
  consume :: a %1 -> ()

class (Consumable a) => Dupable a where
  dup2 :: a %1 -> (a, a)

class (Dupable a) => Movable a where
  move :: a %1 -> Ur a

{- | Consume the unit and return the second argument.
This is like 'seq' but since the first argument is restricted to be of type
@()@ it is consumed, hence @seqUnit@ is linear in its first argument.
-}
seqUnit :: () %1 -> b %1 -> b
seqUnit () b = b

lseq :: (Consumable a) => a %1 -> b %1 -> b
lseq a b = consume a `seqUnit` b

dup :: (Dupable a) => a %1 -> (a, a)
{-# INLINE dup #-}
dup = dup2

newtype AsMovable a = AsMovable a

instance (Movable a) => Consumable (AsMovable a) where
  consume (AsMovable a) = case move a of Ur _ -> ()

instance (Movable a) => Dupable (AsMovable a) where
  dup2 (AsMovable a) = case move a of Ur a' -> (AsMovable a', AsMovable a')

deriving newtype instance (Movable a) => Movable (AsMovable a)

instance Consumable (Ur a) where
  consume Ur {} = ()
  {-# INLINE consume #-}

instance Dupable (Ur a) where
  dup2 (Ur a) = (Ur a, Ur a)
  {-# INLINE dup2 #-}

instance Movable (Ur a) where
  move (Ur a) = Ur (Ur a)
  {-# INLINE move #-}

deriving via AsMovable Int instance Consumable Int

deriving via AsMovable Int instance Dupable Int

instance Movable Int where
  move (I# a) = Unsafe.toLinear (\j -> Ur (I# j)) a

deriving via AsMovable Int8 instance Consumable Int8

deriving via AsMovable Int8 instance Dupable Int8

instance Movable Int8 where
  move (I8# a) = Unsafe.toLinear (\j -> Ur (I8# j)) a

deriving via AsMovable Int16 instance Consumable Int16

deriving via AsMovable Int16 instance Dupable Int16

instance Movable Int16 where
  move (I16# a) = Unsafe.toLinear (\j -> Ur (I16# j)) a

deriving via AsMovable Int32 instance Consumable Int32

deriving via AsMovable Int32 instance Dupable Int32

instance Movable Int32 where
  move (I32# a) = Unsafe.toLinear (\j -> Ur (I32# j)) a

deriving via AsMovable Int64 instance Consumable Int64

deriving via AsMovable Int64 instance Dupable Int64

instance Movable Int64 where
  move (I64# a) = Unsafe.toLinear (\j -> Ur (I64# j)) a

deriving via AsMovable Word instance Consumable Word

deriving via AsMovable Word instance Dupable Word

instance Movable Word where
  move (W# a) = Unsafe.toLinear (\j -> Ur (W# j)) a

deriving via AsMovable Word8 instance Consumable Word8

deriving via AsMovable Word8 instance Dupable Word8

instance Movable Word8 where
  move (W8# a) = Unsafe.toLinear (\j -> Ur (W8# j)) a

deriving via AsMovable Word16 instance Consumable Word16

deriving via AsMovable Word16 instance Dupable Word16

instance Movable Word16 where
  move (W16# a) = Unsafe.toLinear (\j -> Ur (W16# j)) a

deriving via AsMovable Word32 instance Consumable Word32

deriving via AsMovable Word32 instance Dupable Word32

instance Movable Word32 where
  move (W32# a) = Unsafe.toLinear (\j -> Ur (W32# j)) a

deriving via AsMovable Word64 instance Consumable Word64

deriving via AsMovable Word64 instance Dupable Word64

instance Movable Word64 where
  move (W64# a) = Unsafe.toLinear (\j -> Ur (W64# j)) a

deriving via AsMovable Bool instance Consumable Bool

deriving via AsMovable Bool instance Dupable Bool

instance Movable Bool where
  move True = Ur True
  move False = Ur False

deriving newtype instance Consumable Mon.Any

deriving newtype instance Dupable Mon.Any

deriving newtype instance Movable Mon.Any

deriving newtype instance Consumable Mon.All

deriving newtype instance Dupable Mon.All

deriving newtype instance Movable Mon.All

instance Consumable () where
  consume () = ()
  {-# INLINE consume #-}

instance Dupable () where
  dup2 () = ((), ())
  {-# INLINE dup2 #-}

instance Movable () where
  move () = Ur ()
  {-# INLINE move #-}

deriving newtype instance (Consumable a) => Consumable (Mon.Product a)

deriving newtype instance (Dupable a) => Dupable (Mon.Product a)

deriving newtype instance (Movable a) => Movable (Mon.Product a)

deriving newtype instance (Consumable a) => Consumable (Mon.Sum a)

deriving newtype instance (Dupable a) => Dupable (Mon.Sum a)

deriving newtype instance (Movable a) => Movable (Mon.Sum a)

instance (Consumable a) => Consumable (Maybe a) where
  consume Nothing = ()
  consume (Just a) = consume a

instance (Dupable a) => Dupable (Maybe a) where
  dup2 Nothing = (Nothing, Nothing)
  dup2 (Just a) = case dup2 a of (a', a'') -> (Just a', Just a'')

instance (Movable a) => Movable (Maybe a) where
  move Nothing = Ur Nothing
  move (Just a) = lift Just (move a)

instance (Consumable a) => Consumable [a] where
  consume [] = ()
  consume (x : xs) = consume x `seqUnit` consume xs
  {-# INLINE consume #-}

instance (Dupable a) => Dupable [a] where
  dup2 [] = ([], [])
  dup2 (x : xs) = case dup2 x of (x', x'') -> case dup2 xs of (xs', xs'') -> (x' : xs', x'' : xs'')

instance (Movable a) => Movable [a] where
  move [] = Ur []
  move (x : xs) = lift2 (:) (move x) (move xs)

instance (Consumable a, Consumable b) => Consumable (a, b) where
  consume (a, b) = consume a `seqUnit` consume b
  {-# INLINE consume #-}

instance (Dupable a, Dupable b) => Dupable (a, b) where
  dup2 (a, b) = case dup2 a of (a', a'') -> case dup2 b of (b', b'') -> ((a', b'), (a'', b''))

instance (Movable a, Movable b) => Movable (a, b) where
  move (a, b) = lift2 (,) (move a) (move b)
