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
module GHC.Wasm.Web.Generated.SVGElement (
        SVGElement, SVGElementClass, js_fun_focus__undefined,
        js_fun_blur__undefined, js_get_id, js_set_id, js_get_className,
        js_get_dataset, js_get_style, js_get_ownerSVGElement,
        js_get_viewportElement, js_get_tabIndex, js_set_tabIndex,
        js_get_onabort, js_set_onabort, js_get_onblur, js_set_onblur,
        js_get_onfocus, js_set_onfocus, js_get_onauxclick,
        js_set_onauxclick, js_get_oncanplay, js_set_oncanplay,
        js_get_oncanplaythrough, js_set_oncanplaythrough, js_get_onchange,
        js_set_onchange, js_get_onclick, js_set_onclick, js_get_onclose,
        js_set_onclose, js_get_oncontextmenu, js_set_oncontextmenu,
        js_get_ondblclick, js_set_ondblclick, js_get_ondrag, js_set_ondrag,
        js_get_ondragend, js_set_ondragend, js_get_ondragenter,
        js_set_ondragenter, js_get_ondragexit, js_set_ondragexit,
        js_get_ondragleave, js_set_ondragleave, js_get_ondragover,
        js_set_ondragover, js_get_ondragstart, js_set_ondragstart,
        js_get_ondrop, js_set_ondrop, js_get_ondurationchange,
        js_set_ondurationchange, js_get_onemptied, js_set_onemptied,
        js_get_onended, js_set_onended, js_get_oninput, js_set_oninput,
        js_get_oninvalid, js_set_oninvalid, js_get_onkeydown,
        js_set_onkeydown, js_get_onkeypress, js_set_onkeypress,
        js_get_onkeyup, js_set_onkeyup, js_get_onload, js_set_onload,
        js_get_onloadeddata, js_set_onloadeddata, js_get_onloadedmetadata,
        js_set_onloadedmetadata, js_get_onloadend, js_set_onloadend,
        js_get_onloadstart, js_set_onloadstart, js_get_onmousedown,
        js_set_onmousedown, js_get_onmouseenter, js_set_onmouseenter,
        js_get_onmouseleave, js_set_onmouseleave, js_get_onmousemove,
        js_set_onmousemove, js_get_onmouseout, js_set_onmouseout,
        js_get_onmouseover, js_set_onmouseover, js_get_onmouseup,
        js_set_onmouseup, js_get_onwheel, js_set_onwheel, js_get_onpause,
        js_set_onpause, js_get_onplay, js_set_onplay, js_get_onplaying,
        js_set_onplaying, js_get_onprogress, js_set_onprogress,
        js_get_onratechange, js_set_onratechange, js_get_onreset,
        js_set_onreset, js_get_onresize, js_set_onresize, js_get_onscroll,
        js_set_onscroll, js_get_onseeked, js_set_onseeked,
        js_get_onseeking, js_set_onseeking, js_get_onselect,
        js_set_onselect, js_get_onshow, js_set_onshow, js_get_onstalled,
        js_set_onstalled, js_get_onsubmit, js_set_onsubmit,
        js_get_onsuspend, js_set_onsuspend, js_get_ontimeupdate,
        js_set_ontimeupdate, js_get_onvolumechange, js_set_onvolumechange,
        js_get_onwaiting, js_set_onwaiting, js_get_onselectstart,
        js_set_onselectstart, js_get_ontoggle, js_set_ontoggle,
        js_get_onpointercancel, js_set_onpointercancel,
        js_get_onpointerdown, js_set_onpointerdown, js_get_onpointerup,
        js_set_onpointerup, js_get_onpointermove, js_set_onpointermove,
        js_get_onpointerout, js_set_onpointerout, js_get_onpointerover,
        js_set_onpointerover, js_get_onpointerenter, js_set_onpointerenter,
        js_get_onpointerleave, js_set_onpointerleave,
        js_get_ongotpointercapture, js_set_ongotpointercapture,
        js_get_onlostpointercapture, js_set_onlostpointercapture,
        js_get_onanimationcancel, js_set_onanimationcancel,
        js_get_onanimationend, js_set_onanimationend,
        js_get_onanimationiteration, js_set_onanimationiteration,
        js_get_onanimationstart, js_set_onanimationstart,
        js_get_ontransitioncancel, js_set_ontransitioncancel,
        js_get_ontransitionend, js_set_ontransitionend,
        js_get_ontransitionrun, js_set_ontransitionrun,
        js_get_ontransitionstart, js_set_ontransitionstart,
        js_get_onwebkitanimationend, js_set_onwebkitanimationend,
        js_get_onwebkitanimationiteration,
        js_set_onwebkitanimationiteration, js_get_onwebkitanimationstart,
        js_set_onwebkitanimationstart, js_get_onwebkittransitionend,
        js_set_onwebkittransitionend, js_get_oncopy, js_set_oncopy,
        js_get_oncut, js_set_oncut, js_get_onpaste, js_set_onpaste,
        js_get_ontouchstart, js_set_ontouchstart, js_get_ontouchend,
        js_set_ontouchend, js_get_ontouchmove, js_set_ontouchmove,
        js_get_ontouchcancel, js_set_ontouchcancel, js_get_onerror,
        js_set_onerror
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.CSSStyleDeclaration.Core
import GHC.Wasm.Web.Generated.DOMStringMap.Core
import GHC.Wasm.Web.Generated.Element.Core
import GHC.Wasm.Web.Generated.EventHandler.Core
import GHC.Wasm.Web.Generated.SVGAnimatedString.Core
import GHC.Wasm.Web.Generated.SVGElement.Core
import GHC.Wasm.Web.Generated.SVGSVGElement.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.focus()" js_fun_focus__undefined
  :: SVGElement -> (IO ())
foreign import javascript unsafe "$1.blur()" js_fun_blur__undefined
  :: SVGElement -> (IO ())
foreign import javascript unsafe "$1.id" js_get_id
  :: SVGElement -> (IO DOMString)
foreign import javascript unsafe "$1.id = $2" js_set_id
  :: SVGElement -> (DOMString -> (IO ()))
foreign import javascript unsafe "$1.className" js_get_className
  :: SVGElement -> (IO SVGAnimatedString)
foreign import javascript unsafe "$1.dataset" js_get_dataset
  :: SVGElement -> (IO DOMStringMap)
foreign import javascript unsafe "$1.style" js_get_style
  :: SVGElement -> (IO CSSStyleDeclaration)
foreign import javascript unsafe "$1.ownerSVGElement" js_get_ownerSVGElement
  :: SVGElement -> (IO (Nullable SVGSVGElementClass))
foreign import javascript unsafe "$1.viewportElement" js_get_viewportElement
  :: SVGElement -> (IO (Nullable SVGElementClass))
foreign import javascript unsafe "$1.tabIndex" js_get_tabIndex
  :: SVGElement -> (IO Int32)
foreign import javascript unsafe "$1.tabIndex = $2" js_set_tabIndex
  :: SVGElement -> (Int32 -> (IO ()))
foreign import javascript unsafe "$1.onabort" js_get_onabort
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onabort = $2" js_set_onabort
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onblur" js_get_onblur
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onblur = $2" js_set_onblur
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onfocus" js_get_onfocus
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onfocus = $2" js_set_onfocus
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onauxclick" js_get_onauxclick
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onauxclick = $2" js_set_onauxclick
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.oncanplay" js_get_oncanplay
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.oncanplay = $2" js_set_oncanplay
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.oncanplaythrough" js_get_oncanplaythrough
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.oncanplaythrough = $2" js_set_oncanplaythrough
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onchange" js_get_onchange
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onchange = $2" js_set_onchange
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onclick" js_get_onclick
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onclick = $2" js_set_onclick
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onclose" js_get_onclose
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onclose = $2" js_set_onclose
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.oncontextmenu" js_get_oncontextmenu
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.oncontextmenu = $2" js_set_oncontextmenu
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ondblclick" js_get_ondblclick
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.ondblclick = $2" js_set_ondblclick
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ondrag" js_get_ondrag
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.ondrag = $2" js_set_ondrag
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ondragend" js_get_ondragend
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.ondragend = $2" js_set_ondragend
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ondragenter" js_get_ondragenter
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.ondragenter = $2" js_set_ondragenter
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ondragexit" js_get_ondragexit
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.ondragexit = $2" js_set_ondragexit
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ondragleave" js_get_ondragleave
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.ondragleave = $2" js_set_ondragleave
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ondragover" js_get_ondragover
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.ondragover = $2" js_set_ondragover
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ondragstart" js_get_ondragstart
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.ondragstart = $2" js_set_ondragstart
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ondrop" js_get_ondrop
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.ondrop = $2" js_set_ondrop
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ondurationchange" js_get_ondurationchange
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.ondurationchange = $2" js_set_ondurationchange
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onemptied" js_get_onemptied
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onemptied = $2" js_set_onemptied
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onended" js_get_onended
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onended = $2" js_set_onended
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.oninput" js_get_oninput
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.oninput = $2" js_set_oninput
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.oninvalid" js_get_oninvalid
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.oninvalid = $2" js_set_oninvalid
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onkeydown" js_get_onkeydown
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onkeydown = $2" js_set_onkeydown
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onkeypress" js_get_onkeypress
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onkeypress = $2" js_set_onkeypress
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onkeyup" js_get_onkeyup
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onkeyup = $2" js_set_onkeyup
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onload" js_get_onload
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onload = $2" js_set_onload
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onloadeddata" js_get_onloadeddata
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onloadeddata = $2" js_set_onloadeddata
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onloadedmetadata" js_get_onloadedmetadata
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onloadedmetadata = $2" js_set_onloadedmetadata
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onloadend" js_get_onloadend
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onloadend = $2" js_set_onloadend
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onloadstart" js_get_onloadstart
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onloadstart = $2" js_set_onloadstart
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onmousedown" js_get_onmousedown
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onmousedown = $2" js_set_onmousedown
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onmouseenter" js_get_onmouseenter
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onmouseenter = $2" js_set_onmouseenter
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onmouseleave" js_get_onmouseleave
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onmouseleave = $2" js_set_onmouseleave
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onmousemove" js_get_onmousemove
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onmousemove = $2" js_set_onmousemove
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onmouseout" js_get_onmouseout
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onmouseout = $2" js_set_onmouseout
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onmouseover" js_get_onmouseover
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onmouseover = $2" js_set_onmouseover
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onmouseup" js_get_onmouseup
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onmouseup = $2" js_set_onmouseup
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onwheel" js_get_onwheel
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onwheel = $2" js_set_onwheel
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onpause" js_get_onpause
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onpause = $2" js_set_onpause
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onplay" js_get_onplay
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onplay = $2" js_set_onplay
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onplaying" js_get_onplaying
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onplaying = $2" js_set_onplaying
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onprogress" js_get_onprogress
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onprogress = $2" js_set_onprogress
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onratechange" js_get_onratechange
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onratechange = $2" js_set_onratechange
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onreset" js_get_onreset
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onreset = $2" js_set_onreset
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onresize" js_get_onresize
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onresize = $2" js_set_onresize
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onscroll" js_get_onscroll
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onscroll = $2" js_set_onscroll
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onseeked" js_get_onseeked
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onseeked = $2" js_set_onseeked
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onseeking" js_get_onseeking
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onseeking = $2" js_set_onseeking
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onselect" js_get_onselect
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onselect = $2" js_set_onselect
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onshow" js_get_onshow
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onshow = $2" js_set_onshow
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onstalled" js_get_onstalled
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onstalled = $2" js_set_onstalled
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onsubmit" js_get_onsubmit
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onsubmit = $2" js_set_onsubmit
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onsuspend" js_get_onsuspend
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onsuspend = $2" js_set_onsuspend
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ontimeupdate" js_get_ontimeupdate
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.ontimeupdate = $2" js_set_ontimeupdate
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onvolumechange" js_get_onvolumechange
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onvolumechange = $2" js_set_onvolumechange
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onwaiting" js_get_onwaiting
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onwaiting = $2" js_set_onwaiting
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onselectstart" js_get_onselectstart
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onselectstart = $2" js_set_onselectstart
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ontoggle" js_get_ontoggle
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.ontoggle = $2" js_set_ontoggle
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onpointercancel" js_get_onpointercancel
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onpointercancel = $2" js_set_onpointercancel
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onpointerdown" js_get_onpointerdown
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onpointerdown = $2" js_set_onpointerdown
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onpointerup" js_get_onpointerup
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onpointerup = $2" js_set_onpointerup
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onpointermove" js_get_onpointermove
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onpointermove = $2" js_set_onpointermove
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onpointerout" js_get_onpointerout
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onpointerout = $2" js_set_onpointerout
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onpointerover" js_get_onpointerover
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onpointerover = $2" js_set_onpointerover
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onpointerenter" js_get_onpointerenter
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onpointerenter = $2" js_set_onpointerenter
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onpointerleave" js_get_onpointerleave
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onpointerleave = $2" js_set_onpointerleave
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ongotpointercapture" js_get_ongotpointercapture
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.ongotpointercapture = $2" js_set_ongotpointercapture
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onlostpointercapture" js_get_onlostpointercapture
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onlostpointercapture = $2" js_set_onlostpointercapture
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onanimationcancel" js_get_onanimationcancel
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onanimationcancel = $2" js_set_onanimationcancel
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onanimationend" js_get_onanimationend
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onanimationend = $2" js_set_onanimationend
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onanimationiteration" js_get_onanimationiteration
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onanimationiteration = $2" js_set_onanimationiteration
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onanimationstart" js_get_onanimationstart
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onanimationstart = $2" js_set_onanimationstart
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ontransitioncancel" js_get_ontransitioncancel
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.ontransitioncancel = $2" js_set_ontransitioncancel
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ontransitionend" js_get_ontransitionend
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.ontransitionend = $2" js_set_ontransitionend
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ontransitionrun" js_get_ontransitionrun
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.ontransitionrun = $2" js_set_ontransitionrun
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ontransitionstart" js_get_ontransitionstart
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.ontransitionstart = $2" js_set_ontransitionstart
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onwebkitanimationend" js_get_onwebkitanimationend
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onwebkitanimationend = $2" js_set_onwebkitanimationend
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onwebkitanimationiteration" js_get_onwebkitanimationiteration
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onwebkitanimationiteration = $2" js_set_onwebkitanimationiteration
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onwebkitanimationstart" js_get_onwebkitanimationstart
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onwebkitanimationstart = $2" js_set_onwebkitanimationstart
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onwebkittransitionend" js_get_onwebkittransitionend
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onwebkittransitionend = $2" js_set_onwebkittransitionend
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.oncopy" js_get_oncopy
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.oncopy = $2" js_set_oncopy
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.oncut" js_get_oncut
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.oncut = $2" js_set_oncut
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onpaste" js_get_onpaste
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onpaste = $2" js_set_onpaste
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ontouchstart" js_get_ontouchstart
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.ontouchstart = $2" js_set_ontouchstart
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ontouchend" js_get_ontouchend
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.ontouchend = $2" js_set_ontouchend
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ontouchmove" js_get_ontouchmove
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.ontouchmove = $2" js_set_ontouchmove
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ontouchcancel" js_get_ontouchcancel
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.ontouchcancel = $2" js_set_ontouchcancel
  :: SVGElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onerror" js_get_onerror
  :: SVGElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onerror = $2" js_set_onerror
  :: SVGElement -> (EventHandler -> (IO ()))
