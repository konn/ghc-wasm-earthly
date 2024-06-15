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
  -- * Basic Objects and its hierarchy
  JSObject,
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

  -- * Nullable
  NullableClass,
  Nullable,

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
  Prototype,
) where

import Data.Coerce (coerce)
import Data.Kind (Constraint)
import Data.Proxy (Proxy (..))
import Data.Type.Bool (If, type (&&), type (||))
import Data.Void (absurd)
import GHC.Base (Type)
import GHC.Exts (UnliftedType)
import GHC.OverloadedLabels (IsLabel (..))
import GHC.TypeError (ErrorMessage (..), Unsatisfiable)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import GHC.Wasm.Prim (JSString (..), JSVal, fromJSString, toJSString)
import Lens.Family.Total

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

-- NOTE: we limit class tag to be 'UnliftedType' to avoid unintended instance of (<:)
type JSObject :: UnliftedType -> Type
newtype JSObject c = JSObject JSVal

type JSAny = JSObject AnyClass

type data AnyClass :: UnliftedType

type SuperclassOf :: UnliftedType -> Maybe UnliftedType
type family SuperclassOf c

type (<:) :: UnliftedType -> UnliftedType -> Constraint
type sub <: super =
  If
    (sub <:? super)
    (() :: Constraint)
    (Unsatisfiable ('Text "The class " :<>: ShowType sub :<>: Text " is not a subclass of " :<>: ShowType super))

type data UnionClass :: [UnliftedType] -> UnliftedType

type instance SuperclassOf (UnionClass xs) = 'Nothing

type Union cs = JSObject (UnionClass cs)

inject :: (c <: UnionClass cs) => JSObject c -> Union cs
inject = coerce

type data UndefinedClass :: UnliftedType

type instance SuperclassOf UndefinedClass = 'Nothing

type JSUndefined = JSObject UndefinedClass

foreign import javascript safe "undefined"
  js_undefined :: JSUndefined

instance HasJSRep UndefinedClass where
  decodeJSVal _ = pure $ Just js_undefined

type (<:?) :: UnliftedType -> UnliftedType -> Bool
type family sub <:? super where
  sub <:? sub = 'True
  _ <:? AnyClass = 'True
  AnyClass <:? _ = 'False
  NullClass <:? NullableClass a = 'True
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

foreign import javascript safe "typeof $1 === 'string'"
  js_is_string :: JSVal -> Bool

instance (KnownEnum xs) => HasJSRep (EnumClass xs) where
  decodeJSVal jsv =
    if js_is_string jsv
      then pure $ injectEnum @xs $ JSString jsv
      else pure Nothing

type EnumClass :: [Symbol] -> UnliftedType
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

type NullableClass :: UnliftedType -> UnliftedType
type data NullableClass a

type instance SuperclassOf (NullableClass a) = 'Nothing

type Nullable a = JSObject (NullableClass a)

type data NullClass :: UnliftedType

type instance SuperclassOf NullClass = 'Nothing

type JSNull = JSObject NullClass

type Prototype = UnliftedType
