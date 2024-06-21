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
module GHC.Wasm.Web.Generated.SVGTransformList (
        SVGTransformList, SVGTransformListClass, js_fun_clear__undefined,
        js_fun_initialize_SVGTransform_SVGTransform,
        js_fun_insertItemBefore_SVGTransform_long_SVGTransform,
        js_fun_replaceItem_SVGTransform_long_SVGTransform,
        js_fun_removeItem_long_SVGTransform,
        js_fun_appendItem_SVGTransform_SVGTransform,
        js_fun_createSVGTransformFromMatrix_SVGMatrix_SVGTransform,
        js_fun_consolidate__nullable_SVGTransform, js_get_numberOfItems
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.SVGMatrix.Core
import GHC.Wasm.Web.Generated.SVGTransform.Core
import GHC.Wasm.Web.Generated.SVGTransformList.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.clear()" js_fun_clear__undefined
  :: SVGTransformList -> (IO ())
foreign import javascript unsafe "$1.initialize($2)" js_fun_initialize_SVGTransform_SVGTransform
  :: SVGTransformList -> (SVGTransform -> (IO SVGTransform))
foreign import javascript unsafe "$1.insertItemBefore($2,$3)" js_fun_insertItemBefore_SVGTransform_long_SVGTransform
  :: SVGTransformList
     -> (SVGTransform -> (Word32 -> (IO SVGTransform)))
foreign import javascript unsafe "$1.replaceItem($2,$3)" js_fun_replaceItem_SVGTransform_long_SVGTransform
  :: SVGTransformList
     -> (SVGTransform -> (Word32 -> (IO SVGTransform)))
foreign import javascript unsafe "$1.removeItem($2)" js_fun_removeItem_long_SVGTransform
  :: SVGTransformList -> (Word32 -> (IO SVGTransform))
foreign import javascript unsafe "$1.appendItem($2)" js_fun_appendItem_SVGTransform_SVGTransform
  :: SVGTransformList -> (SVGTransform -> (IO SVGTransform))
foreign import javascript unsafe "$1.createSVGTransformFromMatrix($2)" js_fun_createSVGTransformFromMatrix_SVGMatrix_SVGTransform
  :: SVGTransformList -> (SVGMatrix -> (IO SVGTransform))
foreign import javascript unsafe "$1.consolidate()" js_fun_consolidate__nullable_SVGTransform
  :: SVGTransformList -> (IO (Nullable SVGTransformClass))
foreign import javascript unsafe "$1.numberOfItems" js_get_numberOfItems
  :: SVGTransformList -> (IO Word32)
