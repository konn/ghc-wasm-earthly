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
module GHC.Wasm.Web.Generated.Text (
        Text, TextClass, js_cons_Text, js_fun_splitText_long_Text,
        js_fun_getBoxQuads_nullable_BoxQuadOptions_sequence_DOMQuad,
        js_fun_convertQuadFromNode_DOMQuad_GeometryNode_nullable_ConvertCoordinateOptions_DOMQuad,
        js_fun_convertRectFromNode_DOMRectReadOnly_GeometryNode_nullable_ConvertCoordinateOptions_DOMQuad,
        js_fun_convertPointFromNode_DOMPointInit_GeometryNode_nullable_ConvertCoordinateOptions_DOMPoint,
        js_get_wholeText, js_get_assignedSlot
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.BoxQuadOptions.Core
import GHC.Wasm.Web.Generated.CharacterData.Core
import GHC.Wasm.Web.Generated.ConvertCoordinateOptions.Core
import GHC.Wasm.Web.Generated.DOMPoint.Core
import GHC.Wasm.Web.Generated.DOMPointInit.Core
import GHC.Wasm.Web.Generated.DOMQuad.Core
import GHC.Wasm.Web.Generated.DOMRectReadOnly.Core
import GHC.Wasm.Web.Generated.GeometryNode.Core
import GHC.Wasm.Web.Generated.HTMLSlotElement.Core
import GHC.Wasm.Web.Generated.Text.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "new Text($1)" js_cons_Text
  :: Nullable DOMStringClass -> (IO Text)
foreign import javascript unsafe "$1.splitText($2)" js_fun_splitText_long_Text
  :: Text -> (Word32 -> (IO Text))
foreign import javascript unsafe "$1.getBoxQuads($2)" js_fun_getBoxQuads_nullable_BoxQuadOptions_sequence_DOMQuad
  :: Text
     -> (Nullable BoxQuadOptionsClass -> (IO (Sequence DOMQuadClass)))
foreign import javascript unsafe "$1.convertQuadFromNode($2,$3,$4)" js_fun_convertQuadFromNode_DOMQuad_GeometryNode_nullable_ConvertCoordinateOptions_DOMQuad
  :: Text
     -> (DOMQuad
         -> (GeometryNode
             -> (Nullable ConvertCoordinateOptionsClass -> (IO DOMQuad))))
foreign import javascript unsafe "$1.convertRectFromNode($2,$3,$4)" js_fun_convertRectFromNode_DOMRectReadOnly_GeometryNode_nullable_ConvertCoordinateOptions_DOMQuad
  :: Text
     -> (DOMRectReadOnly
         -> (GeometryNode
             -> (Nullable ConvertCoordinateOptionsClass -> (IO DOMQuad))))
foreign import javascript unsafe "$1.convertPointFromNode($2,$3,$4)" js_fun_convertPointFromNode_DOMPointInit_GeometryNode_nullable_ConvertCoordinateOptions_DOMPoint
  :: Text
     -> (DOMPointInit
         -> (GeometryNode
             -> (Nullable ConvertCoordinateOptionsClass -> (IO DOMPoint))))
foreign import javascript unsafe "$1.wholeText" js_get_wholeText
  :: Text -> (IO DOMString)
foreign import javascript unsafe "$1.assignedSlot" js_get_assignedSlot
  :: Text -> (IO (Nullable HTMLSlotElementClass))
