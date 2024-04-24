{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Wasm.Unsafe.Linear (coerce, toLinear, toLinear2, toLinear3) where

import GHC.Base hiding (coerce)
import Unsafe.Coerce

-- | Linearly typed @unsafeCoerce@
coerce :: forall a b. a %1 -> b
{-# INLINE coerce #-}
coerce a = case unsafeEqualityProof @a @b of
  UnsafeRefl -> a

-- | Converts an unrestricted function into a linear function
toLinear ::
  forall
    (r1 :: RuntimeRep)
    (r2 :: RuntimeRep)
    (a :: TYPE r1)
    (b :: TYPE r2)
    p
    x.
  (a %p -> b) %1 ->
  (a %x -> b)
toLinear f = case unsafeEqualityProof @p @x of
  UnsafeRefl -> f

-- | Like 'toLinear' but for two-argument functions
toLinear2 ::
  forall
    (r1 :: RuntimeRep)
    (r2 :: RuntimeRep)
    (r3 :: RuntimeRep)
    (a :: TYPE r1)
    (b :: TYPE r2)
    (c :: TYPE r3)
    p
    q
    x
    y.
  (a %p -> b %q -> c) %1 ->
  (a %x -> b %y -> c)
toLinear2 f = case unsafeEqualityProof @'(p, q) @'(x, y) of
  UnsafeRefl -> f

toLinear3 ::
  forall
    (r1 :: RuntimeRep)
    (r2 :: RuntimeRep)
    (r3 :: RuntimeRep)
    (r4 :: RuntimeRep)
    (a :: TYPE r1)
    (b :: TYPE r2)
    (c :: TYPE r3)
    (d :: TYPE r4)
    p
    q
    r
    x
    y
    z.
  (a %p -> b %q -> c %r -> d) %1 ->
  (a %x -> b %y -> c %z -> d)
toLinear3 f = case unsafeEqualityProof @'(p, q, r) @'(x, y, z) of
  UnsafeRefl -> f
