module Language.WASM.JSVal.Utils (
  isUndefined,
  isNull,
  maybeJSVal,
  DefiniteJSVal (),
  toDefiniteJSVal,
  fromDefiniteJSVal,
  unsafeToDefiniteJSVal,
) where

import Data.Coerce (coerce)
import GHC.Generics
import GHC.Wasm.Prim

-- | A 'JSVal' known to be not @undefined@ or @null@.
newtype DefiniteJSVal = DefiniteJSVal {getDefiniteJSVal :: JSVal}
  deriving (Generic)

unsafeToDefiniteJSVal :: JSVal -> DefiniteJSVal
unsafeToDefiniteJSVal = DefiniteJSVal

fromDefiniteJSVal :: DefiniteJSVal -> JSVal
fromDefiniteJSVal = getDefiniteJSVal

toDefiniteJSVal :: JSVal -> Maybe DefiniteJSVal
toDefiniteJSVal = coerce maybeJSVal

maybeJSVal :: JSVal -> Maybe JSVal
maybeJSVal val
  | isUndefined val = Nothing
  | isNull val = Nothing
  | otherwise = Just val

isUndefined :: JSVal -> Bool
isUndefined = js_is_undefined

foreign import javascript unsafe "$1 === undefined"
  js_is_undefined :: JSVal -> Bool

isNull :: JSVal -> Bool
isNull = js_is_null

foreign import javascript unsafe "$2 === null"
  js_is_null :: JSVal -> Bool
