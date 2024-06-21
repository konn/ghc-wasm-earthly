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
module GHC.Wasm.Web.Generated.File (
        File, FileClass, js_cons_File, js_get_name, js_get_lastModified
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.Blob.Core
import GHC.Wasm.Web.Generated.BlobPart.Core
import GHC.Wasm.Web.Generated.File.Core
import GHC.Wasm.Web.Generated.FilePropertyBag.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "new File($1,$2,$3)" js_cons_File
  :: Sequence BlobPartClass
     -> (USVString -> (Nullable FilePropertyBagClass -> (IO File)))
foreign import javascript unsafe "$1.name" js_get_name
  :: File -> (IO DOMString)
foreign import javascript unsafe "$1.lastModified" js_get_lastModified
  :: File -> (IO Int64)
