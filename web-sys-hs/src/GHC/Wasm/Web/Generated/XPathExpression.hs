{-# OPTIONS_GHC -Wno-all #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UnliftedDatatypes #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE TypeData #-}
module GHC.Wasm.Web.Generated.XPathExpression (
        XPathExpression, XPathExpressionClass,
        js_fun_evaluate_Node_nullable_short_nullable_nullable_object_XPathResult,
        js_fun_evaluateWithContext_Node_long_long_nullable_short_nullable_nullable_object_XPathResult
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.Node.Core
import GHC.Wasm.Web.Generated.XPathExpression.Core
import GHC.Wasm.Web.Generated.XPathResult.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.evaluate($2,$3,$4)" js_fun_evaluate_Node_nullable_short_nullable_nullable_object_XPathResult
  :: XPathExpression
     -> (Node
         -> (Nullable (JSPrimClass Word16)
             -> (Nullable (NullableClass AnyClass) -> (IO XPathResult))))
foreign import javascript unsafe "$1.evaluateWithContext($2,$3,$4,$5,$6)" js_fun_evaluateWithContext_Node_long_long_nullable_short_nullable_nullable_object_XPathResult
  :: XPathExpression
     -> (Node
         -> (Word32
             -> (Word32
                 -> (Nullable (JSPrimClass Word16)
                     -> (Nullable (NullableClass AnyClass) -> (IO XPathResult))))))
