{-# OPTIONS_GHC -fobject-code #-}

module Development.Wasm.Demo.Console (
  consoleLog,
) where

import GHC.Wasm.Prim

foreign import javascript unsafe "console.log($1)"
  js_console_log :: JSString -> IO ()

consoleLog :: String -> IO ()
consoleLog = js_console_log . toJSString
