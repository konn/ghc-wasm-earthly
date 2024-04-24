{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Wasm.Data.Function.Linear (id, const, (.), flip, (&), ($), seq, forget) where

import GHC.Base (TYPE)

id :: a %p -> a
id x = x

const :: a %p -> b -> a
const = \x _ -> x
{-# INLINE const #-}

infixr 9 .

infixr 0 $

($) :: forall {rep} a (b :: TYPE rep) p q. (a %p -> b) %q -> a %p -> b
f $ x = f x
{-# INLINE ($) #-}

(.) :: forall {rep} b (c :: TYPE rep) a q m n. (b %1 -> c) %q -> (a %1 -> b) %m -> a %n -> c
(f . g) x = f (g x)
{-# INLINE (.) #-}

flip :: (a %p -> b %q -> c) %r -> b %q -> a %p -> c
flip = \f y x -> f x y
{-# INLINE flip #-}

infixl 1 &

(&) :: forall {rep} a (b :: TYPE rep) p q. a %p -> (a %p -> b) %q -> b
x & f = f x

infixr 0 `seq`

seq :: a -> b %q -> b
seq a b = a `seq` b

forget :: forall {rep} a (b :: TYPE rep). (a %1 -> b) %1 -> a -> b
forget f a = f a
{-# INLINE forget #-}
