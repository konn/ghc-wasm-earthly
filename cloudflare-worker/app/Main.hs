{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import GHC.Wasm.Prim
import Language.WASM.ByteString qualified as WBS

foreign export javascript "hs_start" main :: IO ()

foreign import javascript unsafe "console.log($1)"
  consoleLog :: JSString -> IO ()

main :: IO ()
main = do
  consoleLog =<< WBS.fromByteString "hello world"
