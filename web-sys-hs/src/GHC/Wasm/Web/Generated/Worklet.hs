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
module GHC.Wasm.Web.Generated.Worklet (
        Worklet, WorkletClass,
        js_fun_addModule_USVString_nullable_WorkletOptions_Promise_undefined
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.Worklet.Core
import GHC.Wasm.Web.Generated.WorkletOptions.Core
import GHC.Wasm.Web.Types
foreign import javascript safe "$1.addModule($2,$3)" js_fun_addModule_USVString_nullable_WorkletOptions_Promise_undefined
  :: Worklet
     -> (USVString
         -> (Nullable WorkletOptionsClass -> (IO (Promise UndefinedClass))))
