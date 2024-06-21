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
module GHC.Wasm.Web.Generated.Directory (
        Directory, DirectoryClass,
        js_fun_getFilesAndDirectories__Promise_sequence_Union_File_Directory_EndUnion,
        js_fun_getFiles_nullable_boolean_Promise_sequence_File,
        js_get_name, js_get_path
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.Directory.Core
import GHC.Wasm.Web.Generated.File.Core
import GHC.Wasm.Web.Types
foreign import javascript safe "$1.getFilesAndDirectories()" js_fun_getFilesAndDirectories__Promise_sequence_Union_File_Directory_EndUnion
  :: Directory
     -> (IO (Promise (SequenceClass (UnionClass '[FileClass,
                                                  DirectoryClass]))))
foreign import javascript safe "$1.getFiles($2)" js_fun_getFiles_nullable_boolean_Promise_sequence_File
  :: Directory
     -> (Nullable (JSPrimClass Bool)
         -> (IO (Promise (SequenceClass FileClass))))
foreign import javascript unsafe "$1.name" js_get_name
  :: Directory -> (IO DOMString)
foreign import javascript unsafe "$1.path" js_get_path
  :: Directory -> (IO DOMString)
