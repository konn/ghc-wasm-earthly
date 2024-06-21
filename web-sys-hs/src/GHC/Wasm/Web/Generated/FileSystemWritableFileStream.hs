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
module GHC.Wasm.Web.Generated.FileSystemWritableFileStream (
        FileSystemWritableFileStream, FileSystemWritableFileStreamClass,
        js_fun_write_FileSystemWriteChunkType_Promise_undefined,
        js_fun_seek_longlong_Promise_undefined,
        js_fun_truncate_longlong_Promise_undefined
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.FileSystemWritableFileStream.Core
import GHC.Wasm.Web.Generated.FileSystemWriteChunkType.Core
import GHC.Wasm.Web.Generated.WritableStream.Core
import GHC.Wasm.Web.Types
foreign import javascript safe "$1.write($2)" js_fun_write_FileSystemWriteChunkType_Promise_undefined
  :: FileSystemWritableFileStream
     -> (FileSystemWriteChunkType -> (IO (Promise UndefinedClass)))
foreign import javascript safe "$1.seek($2)" js_fun_seek_longlong_Promise_undefined
  :: FileSystemWritableFileStream
     -> (Word64 -> (IO (Promise UndefinedClass)))
foreign import javascript safe "$1.truncate($2)" js_fun_truncate_longlong_Promise_undefined
  :: FileSystemWritableFileStream
     -> (Word64 -> (IO (Promise UndefinedClass)))
