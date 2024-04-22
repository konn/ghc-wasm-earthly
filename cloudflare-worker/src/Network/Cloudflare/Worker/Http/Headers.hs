module Network.Cloudflare.Worker.Http.Headers (
  Headers (..),
  newHeaders,
  append,
  delete,
  get,
  has,
  set,
  toEntryStream,
  toMap,
) where

import Control.Monad ((<=<))
import Control.Monad.IO.Class (liftIO)
import Data.Coerce (coerce)
import Data.Map.Strict (Map)
import Data.Text qualified as T
import GHC.Wasm.Prim
import Language.WASM.JSVal.Iterator qualified as Iter
import Language.WASM.JSVal.JSON (fromJSVal)
import Language.WASM.JSVal.Utils (fromDefiniteJSVal, toDefiniteJSVal)
import Streaming.Prelude qualified as S

newtype Headers = Headers JSVal

foreign import javascript unsafe "new Headers()"
  js_new_Headers :: IO Headers

-- | Creates a new Headers object.
newHeaders :: IO Headers
newHeaders = js_new_Headers

foreign import javascript unsafe "$1.append($2, $3)"
  js_Headers_append :: Headers -> JSString -> JSString -> IO ()

-- | Appends a new value onto an existing header inside a Headers object, or adds the header if it does not already exist.
append :: JSString -> JSString -> Headers -> IO ()
append name value headers = js_Headers_append headers name value

foreign import javascript unsafe "$1.delete($2)"
  js_Headers_delete :: Headers -> JSString -> IO ()

-- | Deletes a header from a Headers object.
delete :: JSString -> Headers -> IO ()
delete name headers = js_Headers_delete headers name

foreign import javascript unsafe "$1.get($2)"
  js_Headers_get :: Headers -> JSString -> IO JSString

-- Returns a String sequence of all the values of a header within a Headers object with a given name.
get :: JSString -> Headers -> IO (Maybe JSString)
get name headers =
  fmap (JSString . fromDefiniteJSVal) . toDefiniteJSVal . coerce
    <$> js_Headers_get headers name

foreign import javascript unsafe "$1.has($2)"
  js_Headers_has :: Headers -> JSString -> IO Bool

has :: JSString -> Headers -> IO Bool
has name headers = js_Headers_has headers name

foreign import javascript unsafe "$1.set($2)"
  js_Headers_set :: Headers -> JSString -> JSString -> IO ()

set :: JSString -> JSString -> Headers -> IO ()
set name value headers = js_Headers_set headers name value

toEntryStream :: Headers -> S.Stream (S.Of (JSString, JSString)) IO ()
toEntryStream =
  S.mapM
    (liftA2 (,) <$> js_index_string_array 0 <*> js_index_string_array 1)
    . Iter.toStream
    <=< liftIO . js_Headers_entries

foreign import javascript unsafe "$2[$1]"
  js_index_string_array :: Int -> JSVal -> IO JSString

foreign import javascript unsafe "$1.entries()"
  js_Headers_entries :: Headers -> IO Iter.Iterator

foreign import javascript unsafe "Object.fromEntries($1)"
  js_Headers_to_Object :: Headers -> IO JSVal

toMap :: Headers -> IO (Map T.Text T.Text)
toMap =
  maybe (error "toMap: invalid entries object found") pure
    <=< fromJSVal
    <=< js_Headers_to_Object
