{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE UnliftedDatatypes #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module GHC.Wasm.Object.Core (
  Prototype,

  -- * Basic Objects and its hierarchy
  JSObject (..),
  emptyObject,
  upcast,
  unsafeCast,
  unJSObject,
  SuperclassOf,
  SuperclassOf',
  type (<:),
  type (<:?),
  unsafeAsObject,

  -- * Any
  AnyClass,
  JSAny,

  -- * Undefined
  UndefinedClass,
  JSUndefined,

  -- * Null
  NullClass,
  JSNull,
  jsNull,

  -- * Nullable
  NullableClass,
  Nullable,
  toNullable,
  fromNullable,

  -- * Unions
  UnionClass,
  Union,
  inject,

  -- * Enums
  EnumClass,
  JSEnum,
  enumTag,
  injectEnum,
  onEnum,
  MemberC,
  KnownEnum (),
  FromJSVal (..),
  HasJSRep (..),
  JSPrimClass,
  JSPrim,
  JSPrimitive (..),
  JSSymbolClass,
  JSSymbol,
) where

import Data.Coerce (coerce)
import Data.Int
import Data.Kind (Constraint)
import Data.Proxy (Proxy (..))
import Data.Type.Bool (If, type (&&), type (||))
import Data.Void (absurd)
import Data.Word
import Foreign (Storable)
import GHC.Base (Type)
import GHC.Exts (UnliftedType)
import GHC.OverloadedLabels (IsLabel (..))
import GHC.TypeError (ErrorMessage (..), Unsatisfiable)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import GHC.Wasm.Prim (JSString (..), JSVal, fromJSString, toJSString)
import Lens.Family.Total

type Prototype = UnliftedType

class FromJSVal a where
  fromJSVal :: JSVal -> IO (Maybe a)

instance FromJSVal JSVal where
  fromJSVal = pure . Just

class HasJSRep c where
  decodeJSVal :: JSVal -> IO (Maybe (JSObject c))

instance HasJSRep AnyClass where
  decodeJSVal = pure . Just . JSObject

instance (HasJSRep cls) => FromJSVal (JSObject cls) where
  fromJSVal = decodeJSVal

-- NOTE: we limit class tag to be 'Prototype' to avoid unintended instance of (<:)
type JSObject :: Prototype -> Type
newtype JSObject c = JSObject JSVal

type JSAny = JSObject AnyClass

type data AnyClass :: Prototype

type SuperclassOf :: Prototype -> Maybe Prototype
type family SuperclassOf c

type (<:) :: Prototype -> Prototype -> Constraint
type sub <: super =
  If
    (sub <:? super)
    (() :: Constraint)
    (Unsatisfiable ('Text "The class " :<>: ShowType sub :<>: Text " is not a subclass of " :<>: ShowType super))

type data UnionClass :: [Prototype] -> Prototype

type instance SuperclassOf (UnionClass xs) = 'Nothing

type Union cs = JSObject (UnionClass cs)

inject :: (c <: UnionClass cs) => JSObject c -> Union cs
inject = coerce

type data UndefinedClass :: Prototype

type instance SuperclassOf UndefinedClass = 'Nothing

type JSUndefined = JSObject UndefinedClass

foreign import javascript unsafe "undefined"
  js_undefined :: JSUndefined

instance HasJSRep UndefinedClass where
  decodeJSVal _ = pure $ Just js_undefined

type (<:?) :: Prototype -> Prototype -> Bool
type family sub <:? super where
  a <:? NullableClass a = 'True
  NullClass <:? NullableClass a = 'True
  sub <:? sub = 'True
  _ <:? AnyClass = 'True
  AnyClass <:? _ = 'False
  UnionClass '[] <:? c = 'True
  UnionClass (x ': xs) <:? c = x <:? c && UnionClass xs <:? c
  _ <:? EnumClass '[] = 'False
  EnumClass '[] <:? EnumClass xs = 'True
  EnumClass (x ': xs) <:? EnumClass ys =
    Member x ys && EnumClass xs <:? EnumClass ys
  sub <:? UnionClass '[] = 'False
  sub <:? UnionClass (c ': cs) = sub <:? c || sub <:? UnionClass cs
  sub <:? super = SuperclassOf' sub <:? super

type family Member x xs where
  Member x '[] = 'False
  Member x (x ': xs) = 'True
  Member x (_ ': xs) = Member x xs

type SuperclassOf' a = FromMaybe AnyClass (SuperclassOf a)

type family FromMaybe a m where
  FromMaybe a 'Nothing = a
  FromMaybe _ ('Just a) = a

type MemberC x xs = If (Member x xs) (() :: Constraint) (Unsatisfiable ('ShowType x :<>: Text " is not a member of " :<>: ShowType xs))

unJSObject :: JSObject c -> JSVal
unJSObject = coerce

upcast :: (sub <: super) => JSObject sub -> JSObject super
upcast = coerce

unsafeCast :: JSObject c -> JSObject c'
unsafeCast = coerce

unsafeAsObject :: JSVal -> JSObject c
unsafeAsObject = coerce

foreign import javascript unsafe "typeof $1 === 'string'"
  js_is_string :: JSVal -> Bool

instance (KnownEnum xs) => HasJSRep (EnumClass xs) where
  decodeJSVal jsv =
    if js_is_string jsv
      then pure $ injectEnum @xs $ JSString jsv
      else pure Nothing

type EnumClass :: [Symbol] -> Prototype
type data EnumClass xs

type instance SuperclassOf (EnumClass xs) = 'Nothing

type JSEnum xs = JSObject (EnumClass xs)

enumTag :: JSEnum xs -> JSString
enumTag = JSString . coerce

instance (KnownSymbol s, MemberC s xs) => IsLabel s (JSEnum xs) where
  fromLabel = coerce $ toJSString $ symbolVal $ Proxy @s

type KnownEnum_ :: [Symbol] -> Constraint
type family KnownEnum_ xs where
  KnownEnum_ '[] = ()
  KnownEnum_ (x ': xs) = (KnownSymbol x, KnownEnum xs)

type KnownEnum :: [Symbol] -> Constraint
class (KnownEnum_ xs) => KnownEnum xs where
  syms :: Proxy xs -> [String]

instance KnownEnum '[] where
  syms _ = []

instance (KnownSymbol x, KnownEnum xs) => KnownEnum (x ': xs) where
  syms _ = symbolVal (Proxy @x) : syms (Proxy @xs)

injectEnum :: forall xs. (KnownEnum xs) => JSString -> Maybe (JSEnum xs)
injectEnum s
  | fromJSString s `elem` syms (Proxy @xs) = Just $ coerce s
  | otherwise = Nothing

instance (xs ~ '[]) => Empty (JSEnum xs) where
  impossible = error "impossible"

type family Delete x xs where
  Delete x '[] = '[]
  Delete x (x ': xs) = Delete x xs
  Delete x (y ': xs) = y ': Delete x xs

-- | To be used with 'total'
instance
  {-# INCOHERENT #-}
  (KnownSymbol s, MemberC s xs, targ ~ JSEnum (Delete s xs)) =>
  IsLabel s ((() -> Either () Void) -> JSEnum xs -> Either () targ)
  where
  fromLabel f e =
    let sym = symbolVal (Proxy @s)
     in if sym == fromJSString (enumTag e)
          then absurd =<< f ()
          else Right $ coerce e

-- | Type-safe case analysis for 'JSEnum', to be used with '_case' or '_default' from @total@ package.
onEnum :: ((() -> Either () Void) -> s -> Either () r) -> o -> (r -> o) -> s -> o
onEnum p = on p . const

type NullableClass :: Prototype -> Prototype
type data NullableClass a

type instance SuperclassOf (NullableClass a) = 'Nothing

type Nullable a = JSObject (NullableClass a)

type data NullClass :: Prototype

type instance SuperclassOf NullClass = 'Nothing

type JSNull = JSObject NullClass

jsNull :: JSNull
jsNull = js_null

toNullable :: Maybe (JSObject c) -> Nullable c
toNullable = maybe (unsafeCast js_null) upcast

fromNullable :: Nullable c -> Maybe (JSObject c)
fromNullable n
  | js_is_null n = Nothing
  | otherwise = Just $ unsafeCast n

foreign import javascript unsafe "null"
  js_null :: JSNull

foreign import javascript unsafe "$1 === null"
  js_is_null :: Nullable a -> Bool

type JSPrimClass :: Type -> Prototype
type data JSPrimClass a :: Prototype

type JSPrim a = JSObject (JSPrimClass a)

class (Storable a) => JSPrimitive a where
  toJSPrim :: a -> JSPrim a
  fromJSPrim :: JSPrim a -> a

foreign import javascript unsafe "$1"
  toJSPrim_Bool :: Bool -> JSPrim Bool

foreign import javascript unsafe "$1"
  fromJSPrim_Bool :: JSPrim Bool -> Bool

instance JSPrimitive Bool where
  toJSPrim = toJSPrim_Bool
  fromJSPrim = fromJSPrim_Bool

foreign import javascript unsafe "$1"
  toJSPrim_Int8 :: Int8 -> JSPrim Int8

foreign import javascript unsafe "$1"
  fromJSPrim_Int8 :: JSPrim Int8 -> Int8

instance JSPrimitive Int8 where
  toJSPrim = toJSPrim_Int8
  fromJSPrim = fromJSPrim_Int8

foreign import javascript unsafe "$1"
  toJSPrim_Int16 :: Int16 -> JSPrim Int16

foreign import javascript unsafe "$1"
  fromJSPrim_Int16 :: JSPrim Int16 -> Int16

instance JSPrimitive Int16 where
  toJSPrim = toJSPrim_Int16
  fromJSPrim = fromJSPrim_Int16

foreign import javascript unsafe "$1"
  toJSPrim_Int32 :: Int32 -> JSPrim Int32

foreign import javascript unsafe "$1"
  fromJSPrim_Int32 :: JSPrim Int32 -> Int32

instance JSPrimitive Int32 where
  toJSPrim = toJSPrim_Int32
  fromJSPrim = fromJSPrim_Int32

foreign import javascript unsafe "$1"
  toJSPrim_Int64 :: Int64 -> JSPrim Int64

foreign import javascript unsafe "$1"
  fromJSPrim_Int64 :: JSPrim Int64 -> Int64

instance JSPrimitive Int64 where
  toJSPrim = toJSPrim_Int64
  fromJSPrim = fromJSPrim_Int64

foreign import javascript unsafe "$1"
  toJSPrim_Int :: Int -> JSPrim Int

foreign import javascript unsafe "$1"
  fromJSPrim_Int :: JSPrim Int -> Int

instance JSPrimitive Int where
  toJSPrim = toJSPrim_Int
  fromJSPrim = fromJSPrim_Int

foreign import javascript unsafe "$1"
  toJSPrim_Word8 :: Word8 -> JSPrim Word8

foreign import javascript unsafe "$1"
  fromJSPrim_Word8 :: JSPrim Word8 -> Word8

instance JSPrimitive Word8 where
  toJSPrim = toJSPrim_Word8
  fromJSPrim = fromJSPrim_Word8

foreign import javascript unsafe "$1"
  toJSPrim_Word16 :: Word16 -> JSPrim Word16

foreign import javascript unsafe "$1"
  fromJSPrim_Word16 :: JSPrim Word16 -> Word16

instance JSPrimitive Word16 where
  toJSPrim = toJSPrim_Word16
  fromJSPrim = fromJSPrim_Word16

foreign import javascript unsafe "$1"
  toJSPrim_Word32 :: Word32 -> JSPrim Word32

foreign import javascript unsafe "$1"
  fromJSPrim_Word32 :: JSPrim Word32 -> Word32

instance JSPrimitive Word32 where
  toJSPrim = toJSPrim_Word32
  fromJSPrim = fromJSPrim_Word32

foreign import javascript unsafe "$1"
  toJSPrim_Word64 :: Word64 -> JSPrim Word64

foreign import javascript unsafe "$1"
  fromJSPrim_Word64 :: JSPrim Word64 -> Word64

instance JSPrimitive Word64 where
  toJSPrim = toJSPrim_Word64
  fromJSPrim = fromJSPrim_Word64

foreign import javascript unsafe "$1"
  toJSPrim_Word :: Word -> JSPrim Word

foreign import javascript unsafe "$1"
  fromJSPrim_Word :: JSPrim Word -> Word

instance JSPrimitive Word where
  toJSPrim = toJSPrim_Word
  fromJSPrim = fromJSPrim_Word

foreign import javascript unsafe "$1"
  toJSPrim_Double :: Double -> JSPrim Double

foreign import javascript unsafe "$1"
  fromJSPrim_Double :: JSPrim Double -> Double

instance JSPrimitive Double where
  toJSPrim = toJSPrim_Double
  fromJSPrim = fromJSPrim_Double

foreign import javascript unsafe "$1"
  toJSPrim_Float :: Float -> JSPrim Float

foreign import javascript unsafe "$1"
  fromJSPrim_Float :: JSPrim Float -> Float

instance JSPrimitive Float where
  toJSPrim = toJSPrim_Float
  fromJSPrim = fromJSPrim_Float

data JSSymbolClass :: Prototype

type instance SuperclassOf JSSymbolClass = 'Nothing

type JSSymbol = JSObject JSSymbolClass

foreign import javascript unsafe "Object()"
  emptyObject :: IO JSAny
