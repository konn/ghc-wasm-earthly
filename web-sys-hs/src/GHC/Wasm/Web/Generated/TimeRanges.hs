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
module GHC.Wasm.Web.Generated.TimeRanges (
        TimeRanges, TimeRangesClass, js_fun_start_long_double,
        js_fun_end_long_double, js_get_length
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.TimeRanges.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.start($2)" js_fun_start_long_double
  :: TimeRanges -> (Word32 -> (IO Double))
foreign import javascript unsafe "$1.end($2)" js_fun_end_long_double
  :: TimeRanges -> (Word32 -> (IO Double))
foreign import javascript unsafe "$1.length" js_get_length
  :: TimeRanges -> (IO Word32)
