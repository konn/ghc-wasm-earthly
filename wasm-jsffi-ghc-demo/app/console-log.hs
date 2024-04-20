module Main (main) where

import Development.Wasm.Demo.Console

foreign export javascript "hs_start" main :: IO ()

main :: IO ()
main = consoleLog "Hello, world! (cabalised, JS FFI!)"
