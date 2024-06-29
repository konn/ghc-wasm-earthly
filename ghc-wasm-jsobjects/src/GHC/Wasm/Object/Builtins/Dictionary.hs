{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE UnliftedDatatypes #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module GHC.Wasm.Object.Builtins.Dictionary (
  Fields,
  JSDictionaryClass,
  JSDictionary,
  reifyDictionary,
  reflectDictionary,
  getDictField,
  setDictField,
  Membership (),
  Member (),
  Lookup,
  Lookup',
  emptyDictionary,
  ReifiedDictionary (),
  PartialDictionary (),
  newDictionary,
  setPartialField,
  setReifiedDictField,
  KnownFields,
  membership,
  withMembership,
) where

import Control.Exception (evaluate)
import Control.Lens (ix, (.~))
import Data.Function ((&))
import Data.Kind (Constraint, Type)
import Data.Reflection (Given (..), give)
import Data.Type.Equality
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import GHC.Base (Proxy#, proxy#)
import GHC.Generics
import GHC.Records
import GHC.TypeError
import GHC.TypeLits
import GHC.Wasm.Object.Core
import GHC.Wasm.Prim
import System.IO.Unsafe (unsafeDupablePerformIO)
import qualified Wasm.Data.Array.Destination as DA
import qualified Wasm.Prelude.Linear as PL
import qualified Wasm.Unsafe.Linear as Unsafe

-- TODO: Type-level red-black tree
type Fields = [(Symbol, Prototype)]

data JSDictionaryClass :: Fields -> Prototype

reifyDictionary ::
  forall fs.
  (KnownFields fs) =>
  JSDictionary fs ->
  IO (ReifiedDictionary fs)
reifyDictionary dic =
  ReifiedDictionary
    <$> V.mapM
      ( \(MkSomeMember (_ :: Membership f fs)) ->
          js_get_field dic (toJSString (symbolVal' (proxy# @f)))
      )
      (V.fromList (sMembers @fs))

reflectDictionary ::
  forall fs.
  (KnownFields fs) =>
  ReifiedDictionary fs ->
  IO (JSDictionary fs)
reflectDictionary (ReifiedDictionary arr) = do
  dic <- js_new_object
  V.zipWithM_
    ( \(MkSomeMember (_ :: Membership f fs)) x -> do
        js_set_field dic (toJSString (symbolVal' (proxy# @f))) x
    )
    (V.fromList $ sMembers @fs)
    arr
  pure dic

type instance SuperclassOf (JSDictionaryClass fs) = 'Nothing

type JSDictionary fs = JSObject (JSDictionaryClass fs)

type Membership :: forall a. Symbol -> [(Symbol, a)] -> Type
newtype Membership f fs = Membership {getIndex :: Int}
  deriving (Show)

type family KnownFields' fs :: Constraint where
  KnownFields' '[] = ()
  KnownFields' '[ '(f, v)] = (KnownSymbol f)
  KnownFields' ('(f, v) ': '(g, u) ': fs) = (KnownSymbol f, KnownFields ('(g, u) ': fs))

data SomeMembership fs where
  MkSomeMember :: (KnownSymbol f) => Membership f fs -> SomeMembership fs

class (KnownFields' fs) => KnownFields fs where
  sKeys :: Proxy# fs -> [String]
  sMembers :: [SomeMembership fs]
  sLen :: Proxy# fs -> Int
  gfrom :: Proxy# fs -> V.Vector JSAny -> ToRep fs x
  gto :: Proxy# fs -> ToRep fs x -> DA.DArray JSAny %1 -> ()

instance KnownFields '[] where
  sKeys _ = []
  sMembers = []
  sLen _ = 0
  gfrom _ _ = U1
  gto _ U1 = DA.dropEmpty

instance (KnownSymbol f) => KnownFields '[ '(f, v)] where
  sKeys _ = [symbolVal' (proxy# @f)]
  sMembers = [MkSomeMember @f membership]
  sLen _ = 1
  gfrom _ v = M1 $ K1 $ unsafeCast $ V.head v
  gto _ (M1 (K1 x)) = DA.fill (unsafeCast x)

liftSomeMember ::
  [SomeMembership fs] ->
  [SomeMembership ('(f, v) ': fs)]
liftSomeMember = fmap \(MkSomeMember (m :: Membership f fs)) -> withMembership m $ MkSomeMember @f (Membership $ getIndex m + 1)

instance
  (KnownSymbol f, KnownFields ('(g, u) ': fs)) =>
  KnownFields ('(f, v) ': '(g, u) ': fs)
  where
  sKeys _ = symbolVal' (proxy# @f) : sKeys (proxy# @('(g, u) ': fs))
  sMembers = MkSomeMember @f membership : liftSomeMember (sMembers @('(g, u) ': fs))
  sLen _ = 1 + sLen (proxy# @('(g, u) ': fs))
  gfrom _ v = M1 (K1 (unsafeCast $ V.head v)) :*: gfrom (proxy# @('(g, u) ': fs)) (V.tail v)
  gto _ (M1 (K1 x) :*: xs) da =
    DA.split 1 da PL.& \(l, r) ->
      DA.fill (unsafeCast x) l PL.& \() ->
        gto (proxy# @('(g, u) ': fs)) xs r

instance (KnownFields fs) => Generic (ReifiedDictionary fs) where
  type
    Rep (ReifiedDictionary fs) =
      D1
        (MetaData "ReifiedDictionary" "GHC.Wasm.Object.Builtins.Dictionary" "ghc-wasm-jsobjects" 'False)
        ( C1
            (MetaCons "MkReifiedDictionary" PrefixI 'True)
            (ToRep fs)
        )
  from (ReifiedDictionary arr) = M1 $ M1 $ gfrom (proxy# @fs) arr
  to (M1 (M1 x)) = ReifiedDictionary $ DA.withDArray (sLen (proxy# @fs)) (gto (proxy# @fs) x)

-- FIXME: Balance this
type ToRep :: Fields -> Type -> Type
type family ToRep fs where
  ToRep '[] = U1
  ToRep '[ '(k, v)] =
    S1
      ('MetaSel ('Just k) NoSourceUnpackedness NoSourceStrictness DecidedLazy)
      (K1 R (JSObject v))
  ToRep ('(k, v) ': fs) =
    S1
      ('MetaSel ('Just k) NoSourceUnpackedness NoSourceStrictness DecidedLazy)
      (K1 R (JSObject v))
      :*: ToRep fs

type family FromMaybe err m where
  FromMaybe msg 'Nothing = TypeError msg
  FromMaybe _ ('Just a) = a

type Lookup' f fs = FromMaybe ('Text "Key not found: " ':<>: 'ShowType f ':<>: 'Text " absent in " ':<>: 'ShowType fs) (Lookup f fs)

type family Lookup f fs where
  Lookup _ '[] = 'Nothing
  Lookup f ('(f, a) ': fs) = 'Just a
  Lookup f ('(g, a) ': fs) = Lookup f fs

type PartialDictionary :: Fields -> [Symbol] -> Type
newtype PartialDictionary fs gs = DB (MV.MVector MV.RealWorld JSAny)

type family Keys fs where
  Keys '[] = '[]
  Keys ('(f, v) ': fs) = f ': Keys fs

type family RequiredKeys fs where
  RequiredKeys '[] = '[]
  RequiredKeys ('(f, NullableClass v) ': fs) = RequiredKeys fs
  RequiredKeys ('(f, v) ': fs) = f ': RequiredKeys fs

newDictionary ::
  forall fs.
  (KnownFields fs) =>
  (PartialDictionary fs (RequiredKeys fs) %1 -> PartialDictionary fs '[]) %1 ->
  ReifiedDictionary fs
{-# INLINE newDictionary #-}
newDictionary = Unsafe.toLinear \k -> unsafeDupablePerformIO do
  v <- MV.new (sLen (proxy# @fs))
  DB v' <- evaluate $ k (DB v)
  ReifiedDictionary <$> V.unsafeFreeze v'

type family Delete k ks where
  Delete k '[] = '[]
  Delete k (k ': ks) = ks
  Delete k (j ': ks) = j ': Delete k ks

setReifiedDictField ::
  forall fs x.
  forall f ->
  (Member f fs, x ~~ Lookup' f fs) =>
  JSObject x ->
  ReifiedDictionary fs ->
  ReifiedDictionary fs
setReifiedDictField f x (ReifiedDictionary v) =
  ReifiedDictionary $ v & ix (getIndex $ membership @f @fs) .~ unsafeCast x

setPartialField ::
  forall fs gs x.
  forall f ->
  (Member f fs, x ~~ Lookup' f fs) =>
  JSObject x ->
  PartialDictionary fs gs %1 ->
  PartialDictionary fs (Delete f gs)
{-# NOINLINE setPartialField #-}
setPartialField f x =
  Unsafe.toLinear \(DB v) -> unsafeDupablePerformIO do
    () <- MV.write v (getIndex (membership @f @fs)) (unsafeCast x)
    pure $ DB v

emptyDictionary :: ReifiedDictionary '[]
emptyDictionary = ReifiedDictionary V.empty

type ReifiedDictionary :: Fields -> Type
newtype ReifiedDictionary fs = ReifiedDictionary (V.Vector JSAny)

instance
  (Member l fs, Lookup' l fs ~ p) =>
  HasField l (ReifiedDictionary fs) (JSObject p)
  where
  getField (ReifiedDictionary arr) =
    unsafeCast $
      arr V.! getIndex (membership @l @fs)

membership :: (Member f fs) => Membership f fs
membership = given

withMembership :: (KnownSymbol f) => Membership f fs -> ((Member f fs) => r) -> r
withMembership = give

class (Given (Membership f fs), KnownSymbol f) => Member f fs

instance (Given (Membership f fs), KnownSymbol f) => Member f fs

instance (Unsatisfiable ('Text "Empty list")) => Given (Membership f '[]) where
  given = unsatisfiable

instance {-# OVERLAPPING #-} Given (Membership f ('(f, a) ': fs)) where
  given = Membership 0

instance
  {-# OVERLAPPABLE #-}
  (Given (Membership f fs), (f == g) ~ 'False) =>
  Given (Membership f ('(g, a) ': fs))
  where
  given = Membership $ getIndex (given @(Membership f fs)) + 1

getDictField :: forall fs. forall f -> (Member f fs) => JSDictionary fs -> IO (JSObject (Lookup' f fs))
getDictField f dic = unsafeCast <$> js_get_field dic (toJSString (symbolVal' (proxy# @f)))

setDictField ::
  forall fs x.
  forall f ->
  (KnownSymbol f, Member f fs, x ~~ Lookup' f fs) =>
  JSObject x ->
  JSDictionary fs ->
  IO ()
setDictField f v dic =
  js_set_field dic (toJSString (symbolVal' (proxy# @f))) (unsafeCast v)

foreign import javascript unsafe "$1[$2]"
  js_get_field :: JSDictionary fs -> JSString -> IO JSAny

foreign import javascript unsafe "$1[$2] = $3"
  js_set_field :: JSDictionary fs -> JSString -> JSAny -> IO ()

foreign import javascript unsafe "{}"
  js_new_object :: IO (JSDictionary fs)
