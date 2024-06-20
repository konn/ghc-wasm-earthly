{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Wasm.JSVal.Freeze (FrozenJSVal (..), freeze, get) where

import Data.Maybe (Maybe (..))
import GHC.Wasm.Prim
import qualified System.IO as System
import Wasm.Data.Function.Linear
import qualified Wasm.System.IO.Linear as LIO
import qualified Wasm.Unsafe.Linear as Unsafe
import Prelude (Bool, String, error)

-- | An immutable 'JSVal', which is frozen by 'freeze'.
newtype FrozenJSVal = FrozenJSVal JSVal

foreign import javascript unsafe "$1.freeze()"
  js_freeze :: JSVal %1 -> System.IO FrozenJSVal

freeze :: JSVal %1 -> LIO.IO FrozenJSVal
freeze = Unsafe.coerce . js_freeze

foreign import javascript unsafe "$1[$2]"
  js_get :: FrozenJSVal -> JSString -> FrozenJSVal

get :: FrozenJSVal %1 -> String -> (Maybe FrozenJSVal, FrozenJSVal)
get = Unsafe.toLinear \js key ->
  (checkNullUndefined $ js_get js $ toJSString key, js)

foreign import javascript unsafe "$1 === null || $1 === undefined"
  js_null_or_undefined :: FrozenJSVal -> Bool

checkNullUndefined :: FrozenJSVal -> Maybe FrozenJSVal
checkNullUndefined = Unsafe.toLinear \jsv ->
  if js_null_or_undefined jsv
    then Nothing
    else Just jsv
