module GHC.Wasm.FFI.Plugin (plugin) where

import GHC.Plugins

plugin :: Plugin
plugin = defaultPlugin {pluginRecompile = purePlugin}
