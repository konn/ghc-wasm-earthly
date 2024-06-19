module GHC.Wasm.Web.Types (DOMTimeStamp, DOMTimeStampClass) where

import Data.Word
import GHC.Wasm.Object.Builtins

type DOMTimeStamp = Word64

type DOMTimeStampClass = JSPrimClass Word64
