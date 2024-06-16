{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnliftedDatatypes #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module GHC.Wasm.Object.Builtins.BigInt (
  -- * 'DOMString's
  BigInt,
  BigIntClass,
) where

import GHC.Wasm.Object.Core
import GHC.Wasm.Prim

-- | A WebIDL @BigInt@ class, which corresponds to a multi-precision integers.
type data BigIntClass :: Prototype

type instance SuperclassOf BigIntClass = 'Nothing

type BigInt = JSObject BigIntClass

foreign import javascript unsafe "$1 + $2"
  js_add_bigint :: BigInt -> BigInt -> BigInt

foreign import javascript unsafe "$1 - $2"
  js_sub_bigint :: BigInt -> BigInt -> BigInt

foreign import javascript unsafe "$1 * $2"
  js_mul_bigint :: BigInt -> BigInt -> BigInt

foreign import javascript unsafe "$1 / $2"
  js_div_bigint :: BigInt -> BigInt -> BigInt

foreign import javascript unsafe "$1 % $2"
  js_mod_bigint :: BigInt -> BigInt -> BigInt

foreign import javascript unsafe "$1 < $2"
  js_lt_bigint :: BigInt -> BigInt -> Bool

foreign import javascript unsafe "$1 > $2"
  js_gt_bigint :: BigInt -> BigInt -> Bool

foreign import javascript unsafe "$1 <= $2"
  js_leq_bigint :: BigInt -> BigInt -> Bool

foreign import javascript unsafe "$1 >= $2"
  js_geq_bigint :: BigInt -> BigInt -> Bool

foreign import javascript unsafe "$1 == $2"
  js_eq_bigint :: BigInt -> BigInt -> Bool

foreign import javascript unsafe "if ($1 < 0n) { return -1n * $1; } else { return $1; }"
  js_abs_bigint :: BigInt -> BigInt

foreign import javascript unsafe "if ($1 < 0n) { return -1n; } else if ($1 == 0) { return 0n; } else { return 1n; }"
  js_signum_bigint :: BigInt -> BigInt

foreign import javascript unsafe "BigInt($1)"
  js_fromInteger_bigint :: JSString -> BigInt

foreign import javascript unsafe "$1 - 1n"
  js_pred_bigint :: BigInt -> BigInt

foreign import javascript unsafe "$1 + 1n"
  js_succ_bigint :: BigInt -> BigInt

foreign import javascript unsafe "$1.toString()"
  js_bigint_show :: BigInt -> JSString

instance Num BigInt where
  (+) = js_add_bigint
  (-) = js_sub_bigint
  (*) = js_mul_bigint
  abs = js_abs_bigint
  signum = js_signum_bigint
  fromInteger = js_fromInteger_bigint . toJSString . show

instance Enum BigInt where
  pred = js_pred_bigint
  succ = js_succ_bigint
  toEnum = fromInteger . toEnum
  fromEnum = read . fromJSString . js_bigint_show

instance Eq BigInt where
  (==) = js_eq_bigint

instance Ord BigInt where
  (<) = js_lt_bigint
  (>) = js_gt_bigint
  (<=) = js_leq_bigint
  (>=) = js_geq_bigint

instance Show BigInt where
  showsPrec = const $ showString . fromJSString . js_bigint_show

instance Real BigInt where
  toRational = toRational . toInteger

instance Integral BigInt where
  toInteger = read . fromJSString . js_bigint_show
  l `quotRem` r =
    (l `js_div_bigint` r, l `js_mod_bigint` r)
