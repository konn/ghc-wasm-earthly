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
module GHC.Wasm.Web.Generated.MIDIOptions.Core (
        MIDIOptionsFields, MIDIOptionsClass, MIDIOptions,
        ReifiedMIDIOptions
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Types
type MIDIOptionsFields =
    '[ '("software", NullableClass (JSPrimClass Bool)),
       '("sysex", NullableClass (JSPrimClass Bool))]
type MIDIOptionsClass = JSDictionaryClass MIDIOptionsFields
type MIDIOptions = JSObject MIDIOptionsClass
type ReifiedMIDIOptions = ReifiedDictionary MIDIOptionsFields
