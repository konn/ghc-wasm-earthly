module Main (main) where

import Development.Wasm.Demo.Console

main :: IO ()
main = consoleLog "Hello, world! (cabalised, JS FFI!)"
