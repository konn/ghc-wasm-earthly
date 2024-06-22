{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnliftedDatatypes #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Network.Cloudflare.Worker.Binding (
  Bindings,
  BindingsClass,
  getEnv,
  getSecret,
  getBinding,
) where

import Data.Aeson qualified as J
import Data.Kind (Constraint)
import Data.Maybe (fromJust, fromMaybe)
import GHC.Exts (proxy#)
import GHC.TypeError (Unsatisfiable)
import GHC.TypeLits
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Prim
import GHC.Wasm.Web.JSON (JSONClass, decodeJSON)
import System.IO.Unsafe (unsafePerformIO)

type data
  BindingsClass
    (envs :: [Symbol])
    (secrets :: [Symbol])
    (bindingss :: [(Symbol, Prototype)]) ::
    Prototype

type instance SuperclassOf (BindingsClass envs secrets bindingss) = 'Nothing

type Bindings envs secrets bindingss = JSObject (BindingsClass envs secrets bindingss)

type ListMember l ls = ListMemberAux ('ShowType l ':<>: 'Text " is not a member of " ':<>: 'ShowType ls) l ls

type family ListMemberAux (err :: ErrorMessage) (l :: Symbol) (ls :: [Symbol]) :: Constraint where
  ListMemberAux err l '[] = Unsatisfiable err
  ListMemberAux _ l (l ': _) = ()
  ListMemberAux err l (_ ': ls) = ListMemberAux err l ls

getEnv ::
  forall envs secrets bindingss.
  forall l ->
  (KnownSymbol l, ListMember l envs) =>
  Bindings envs secrets bindingss ->
  J.Value
getEnv l b =
  let name = symbolVal' @l proxy#
   in fromJust $
        unsafePerformIO $
          decodeJSON $
            fromMaybe (error $ "No env found: " <> name) $
              fromNullable $
                js_get_env b $
                  toJSString name

getSecret ::
  forall envs secrets bindingss.
  forall l ->
  (KnownSymbol l, ListMember l secrets) =>
  Bindings envs secrets bindingss ->
  String
getSecret l b =
  let key = symbolVal' @l proxy#
   in maybe (error $ "Secret not found: " <> show key) (fromJSString . convertToJSString) $
        fromNullable $
          js_get_secret b $
            toJSString key

getBinding ::
  forall envs secrets bindings.
  forall l ->
  (KnownSymbol l, Member l bindings) =>
  Bindings envs secrets bindings ->
  JSObject (Lookup' l bindings)
getBinding l b =
  let name = symbolVal' @l proxy#
   in fromMaybe (error $ "Not binding found: " <> name) $
        fromNullable $
          js_get_binding b $
            toJSString name

foreign import javascript unsafe "$1[$2]"
  js_get_env :: Bindings envs secrets bindingss -> JSString -> Nullable JSONClass

foreign import javascript unsafe "$1[$2]"
  js_get_secret :: Bindings envs secrets bindingss -> JSString -> Nullable USVStringClass

foreign import javascript unsafe "$1[$2]"
  js_get_binding :: Bindings envs secrets bindingss -> JSString -> Nullable a
