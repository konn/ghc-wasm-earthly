module Main where

import System.Info

main :: IO ()
main = putStrLn $ "Hello, WASM World from GHC 9.10! (" <> show [os, arch, compilerName] <> ")"
