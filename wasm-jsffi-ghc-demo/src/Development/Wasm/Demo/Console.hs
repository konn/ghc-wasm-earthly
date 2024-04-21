module Development.Wasm.Demo.Console (
  consoleLog,
) where

import GHC.Wasm.FFI.Compat

foreign import javascript unsafe "console.log($1)"
  js_console_log :: JSString -> IO ()

consoleLog :: String -> IO ()
consoleLog = js_console_log . toJSString
