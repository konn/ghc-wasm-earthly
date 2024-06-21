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
module GHC.Wasm.Web.Generated.XPathNSResolver.Core (
        XPathNSResolverClass, XPathNSResolver,
        js_mk_callback_XPathNSResolver
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Types
type data XPathNSResolverClass :: Prototype
type instance SuperclassOf XPathNSResolverClass = 'Nothing
type XPathNSResolver = JSObject XPathNSResolverClass
foreign import javascript unsafe "wrapper" js_mk_callback_XPathNSResolver
  :: (Nullable DOMStringClass -> (IO (Nullable DOMStringClass)))
     -> XPathNSResolver
