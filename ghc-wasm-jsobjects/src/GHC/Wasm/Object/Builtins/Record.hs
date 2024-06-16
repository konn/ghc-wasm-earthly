{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnliftedDatatypes #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module GHC.Wasm.Object.Builtins.Record (
  -- * 'DOMString's
  JSRecord,
  JSRecordClass,
  fromJSRecord,
  toJSRecord,
) where

import Control.Monad ((<=<))
import Data.Foldable (forM_)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import qualified Data.Text as T
import GHC.Wasm.Object.Builtins.Sequence
import GHC.Wasm.Object.Builtins.String (DOMStringClass, IsJavaScriptString (..))
import GHC.Wasm.Object.Core
import GHC.Wasm.Prim
import qualified Streaming.Prelude as S

-- | A WebIDL @DOMString@ class, which corresponds to a JavaScript string.
type data JSRecordClass :: Prototype -> Prototype -> Prototype

type instance SuperclassOf (JSRecordClass k v) = 'Nothing

type JSRecord k v = JSObject (JSRecordClass k v)

fromJSRecord ::
  (IsJavaScriptString k) =>
  JSRecord k v ->
  IO (Map T.Text (JSObject v))
fromJSRecord obj = do
  names <- getOwnProperties obj
  Map.fromList <$> mapM (\n -> (T.pack n,) <$> js_get_property obj (fromJust $ convertFromJSString $ toJSString n)) names

toJSRecord ::
  (IsJavaScriptString k) =>
  Map T.Text (JSObject v) ->
  IO (JSRecord k v)
toJSRecord dic = do
  record <- js_new_record
  forM_ (Map.toList dic) \(k, v) -> do
    js_set_property record (fromJust $ convertFromJSString $ toJSString $ T.unpack k) v
  pure record

getOwnProperties :: JSObject a -> IO [String]
getOwnProperties =
  S.toList_
    . S.map (fromJSString . convertToJSString)
    . fromSequence
    <=< js_properties

foreign import javascript unsafe "Object.getOwnPropertyNames($1)"
  js_properties :: JSObject a -> IO (Sequence DOMStringClass)

foreign import javascript unsafe "$1[$2]"
  js_get_property :: JSRecord k a -> JSObject k -> IO (JSObject a)

foreign import javascript unsafe "$1[$2] = $3"
  js_set_property :: JSRecord k v -> JSObject k -> JSObject v -> IO ()

foreign import javascript unsafe "{}"
  js_new_record :: IO (JSRecord k v)
