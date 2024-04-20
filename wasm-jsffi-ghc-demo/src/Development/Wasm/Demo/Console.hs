{-# LANGUAGE TemplateHaskell #-}

module Development.Wasm.Demo.Console (
  consoleLog,
) where

import GHC.Wasm.FFI.Compat

foreignImportJS Safe "console.log" "js_console_log" [t|JSString -> IO ()|]

consoleLog :: String -> IO ()
consoleLog = js_console_log . toJSString
