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
module GHC.Wasm.Web.Generated.MIDIOutput (
        MIDIOutput, MIDIOutputClass,
        js_fun_send_sequence_octet_nullable_DOMHighResTimeStamp_undefined,
        js_fun_clear__undefined
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.DOMHighResTimeStamp.Core
import GHC.Wasm.Web.Generated.MIDIOutput.Core
import GHC.Wasm.Web.Generated.MIDIPort.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.send($2,$3)" js_fun_send_sequence_octet_nullable_DOMHighResTimeStamp_undefined
  :: MIDIOutput
     -> (Sequence (JSPrimClass Word8)
         -> (Nullable DOMHighResTimeStampClass -> (IO ())))
foreign import javascript unsafe "$1.clear()" js_fun_clear__undefined
  :: MIDIOutput -> (IO ())
