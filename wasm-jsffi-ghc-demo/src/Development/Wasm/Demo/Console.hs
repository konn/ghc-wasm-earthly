{-# LANGUAGE TemplateHaskell #-}

module Development.Wasm.Demo.Console (
  consoleLog,
) where

import GHC.Wasm.FFI.Compat

foreign import javascript safe "console.log" js_console_log :: JSString -> IO ()

consoleLog :: String -> IO ()
consoleLog = js_console_log . toJSString
