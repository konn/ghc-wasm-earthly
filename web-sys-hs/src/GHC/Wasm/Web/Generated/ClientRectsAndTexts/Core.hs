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
module GHC.Wasm.Web.Generated.ClientRectsAndTexts.Core (
        ClientRectsAndTextsFields, ClientRectsAndTextsClass,
        ClientRectsAndTexts, ReifiedClientRectsAndTexts
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.DOMRectList.Core
import GHC.Wasm.Web.Types
type ClientRectsAndTextsFields =
    '[ '("rectList", DOMRectListClass),
       '("textList", SequenceClass DOMStringClass)]
type ClientRectsAndTextsClass =
    JSDictionaryClass ClientRectsAndTextsFields
type ClientRectsAndTexts = JSObject ClientRectsAndTextsClass
type ReifiedClientRectsAndTexts =
    ReifiedDictionary ClientRectsAndTextsFields
