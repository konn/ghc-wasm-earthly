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
module GHC.Wasm.Web.Generated.DOMImplementation (
        DOMImplementation, DOMImplementationClass,
        js_fun_hasFeature__boolean,
        js_fun_createDocumentType_DOMString_DOMString_DOMString_DocumentType,
        js_fun_createDocument_nullable_DOMString_DOMString_nullable_nullable_DocumentType_Document,
        js_fun_createHTMLDocument_nullable_DOMString_Document
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.DOMImplementation.Core
import GHC.Wasm.Web.Generated.Document.Core
import GHC.Wasm.Web.Generated.DocumentType.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.hasFeature()" js_fun_hasFeature__boolean
  :: DOMImplementation -> (IO Bool)
foreign import javascript unsafe "$1.createDocumentType($2,$3,$4)" js_fun_createDocumentType_DOMString_DOMString_DOMString_DocumentType
  :: DOMImplementation
     -> (DOMString -> (DOMString -> (DOMString -> (IO DocumentType))))
foreign import javascript unsafe "$1.createDocument($2,$3,$4)" js_fun_createDocument_nullable_DOMString_DOMString_nullable_nullable_DocumentType_Document
  :: DOMImplementation
     -> (Nullable DOMStringClass
         -> (DOMString
             -> (Nullable (NullableClass DocumentTypeClass) -> (IO Document))))
foreign import javascript unsafe "$1.createHTMLDocument($2)" js_fun_createHTMLDocument_nullable_DOMString_Document
  :: DOMImplementation -> (Nullable DOMStringClass -> (IO Document))
