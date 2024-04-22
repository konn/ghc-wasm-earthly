module Language.WASM.JSVal.Iterator (
  Iterator (..),
  IteratorResult (..),
  next,
  pop,
  toStream,
) where

import Data.Functor.Of (Of)
import GHC.Wasm.Prim
import Streaming.Prelude qualified as S

newtype Iterator = Iterator JSVal

data IteratorResult = IteratorResult {done :: Bool, value :: JSVal}

toStream :: Iterator -> S.Stream (Of JSVal) IO ()
toStream = S.reread pop

pop :: Iterator -> IO (Maybe JSVal)
pop iter = do
  result <- next iter
  if done result
    then pure Nothing
    else pure $ Just $ value result

next :: Iterator -> IO IteratorResult
next iter = do
  result <- js_Iterator_next_no_yield iter
  done <- js_get_done result
  value <- js_get_value result
  pure $ IteratorResult done value

foreign import javascript unsafe "$1.done"
  js_get_done :: JSVal -> IO Bool

foreign import javascript unsafe "$1.value"
  js_get_value :: JSVal -> IO JSVal

foreign import javascript unsafe "$1.next()"
  js_Iterator_next_no_yield :: Iterator -> IO JSVal
