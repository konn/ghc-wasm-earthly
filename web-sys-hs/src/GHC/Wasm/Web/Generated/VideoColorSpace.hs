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
module GHC.Wasm.Web.Generated.VideoColorSpace (
        VideoColorSpace, VideoColorSpaceClass, js_cons_VideoColorSpace,
        js_fun_toJSON__VideoColorSpaceInit, js_get_primaries,
        js_get_transfer, js_get_matrix, js_get_fullRange
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.VideoColorPrimaries.Core
import GHC.Wasm.Web.Generated.VideoColorSpace.Core
import GHC.Wasm.Web.Generated.VideoColorSpaceInit.Core
import GHC.Wasm.Web.Generated.VideoMatrixCoefficients.Core
import GHC.Wasm.Web.Generated.VideoTransferCharacteristics.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "new VideoColorSpace($1)" js_cons_VideoColorSpace
  :: Nullable VideoColorSpaceInitClass -> (IO VideoColorSpace)
foreign import javascript unsafe "$1.toJSON()" js_fun_toJSON__VideoColorSpaceInit
  :: VideoColorSpace -> (IO VideoColorSpaceInit)
foreign import javascript unsafe "$1.primaries" js_get_primaries
  :: VideoColorSpace -> (IO (Nullable VideoColorPrimariesClass))
foreign import javascript unsafe "$1.transfer" js_get_transfer
  :: VideoColorSpace
     -> (IO (Nullable VideoTransferCharacteristicsClass))
foreign import javascript unsafe "$1.matrix" js_get_matrix
  :: VideoColorSpace -> (IO (Nullable VideoMatrixCoefficientsClass))
foreign import javascript unsafe "$1.fullRange" js_get_fullRange
  :: VideoColorSpace -> (IO (Nullable (JSPrimClass Bool)))
