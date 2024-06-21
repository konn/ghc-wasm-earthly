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
module GHC.Wasm.Web.Generated.HTMLElement (
        HTMLElement, HTMLElementClass, js_fun_click__undefined,
        js_fun_focus__undefined, js_fun_blur__undefined, js_get_title,
        js_set_title, js_get_scrollHeight, js_set_scrollHeight,
        js_get_scrollTop, js_set_scrollTop, js_get_lang, js_set_lang,
        js_get_dir, js_set_dir, js_get_dataset, js_get_innerText,
        js_set_innerText, js_get_hidden, js_set_hidden, js_get_inert,
        js_set_inert, js_get_tabIndex, js_set_tabIndex, js_get_accessKey,
        js_set_accessKey, js_get_accessKeyLabel, js_get_draggable,
        js_set_draggable, js_get_contentEditable, js_set_contentEditable,
        js_get_isContentEditable, js_get_spellcheck, js_set_spellcheck,
        js_get_style, js_get_offsetParent, js_get_offsetTop,
        js_get_offsetLeft, js_get_offsetWidth, js_get_offsetHeight,
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
import GHC.Wasm.Web.Generated.HTMLElement.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.click()" js_fun_click__undefined
  :: HTMLElement -> (IO ())
foreign import javascript unsafe "$1.focus()" js_fun_focus__undefined
  :: HTMLElement -> (IO ())
foreign import javascript unsafe "$1.blur()" js_fun_blur__undefined
  :: HTMLElement -> (IO ())
foreign import javascript unsafe "$1.title" js_get_title
  :: HTMLElement -> (IO DOMString)
foreign import javascript unsafe "$1.title = $2" js_set_title
  :: HTMLElement -> (DOMString -> (IO ()))
foreign import javascript unsafe "$1.scrollHeight" js_get_scrollHeight
  :: HTMLElement -> (IO Int32)
foreign import javascript unsafe "$1.scrollHeight = $2" js_set_scrollHeight
  :: HTMLElement -> (Int32 -> (IO ()))
foreign import javascript unsafe "$1.scrollTop" js_get_scrollTop
  :: HTMLElement -> (IO Int32)
foreign import javascript unsafe "$1.scrollTop = $2" js_set_scrollTop
  :: HTMLElement -> (Int32 -> (IO ()))
foreign import javascript unsafe "$1.lang" js_get_lang
  :: HTMLElement -> (IO DOMString)
foreign import javascript unsafe "$1.lang = $2" js_set_lang
  :: HTMLElement -> (DOMString -> (IO ()))
foreign import javascript unsafe "$1.dir" js_get_dir
  :: HTMLElement -> (IO DOMString)
foreign import javascript unsafe "$1.dir = $2" js_set_dir
  :: HTMLElement -> (DOMString -> (IO ()))
foreign import javascript unsafe "$1.dataset" js_get_dataset
  :: HTMLElement -> (IO DOMStringMap)
foreign import javascript unsafe "$1.innerText" js_get_innerText
  :: HTMLElement -> (IO DOMString)
foreign import javascript unsafe "$1.innerText = $2" js_set_innerText
  :: HTMLElement -> (DOMString -> (IO ()))
foreign import javascript unsafe "$1.hidden" js_get_hidden
  :: HTMLElement -> (IO Bool)
foreign import javascript unsafe "$1.hidden = $2" js_set_hidden
  :: HTMLElement -> (Bool -> (IO ()))
foreign import javascript unsafe "$1.inert" js_get_inert
  :: HTMLElement -> (IO Bool)
foreign import javascript unsafe "$1.inert = $2" js_set_inert
  :: HTMLElement -> (Bool -> (IO ()))
foreign import javascript unsafe "$1.tabIndex" js_get_tabIndex
  :: HTMLElement -> (IO Int32)
foreign import javascript unsafe "$1.tabIndex = $2" js_set_tabIndex
  :: HTMLElement -> (Int32 -> (IO ()))
foreign import javascript unsafe "$1.accessKey" js_get_accessKey
  :: HTMLElement -> (IO DOMString)
foreign import javascript unsafe "$1.accessKey = $2" js_set_accessKey
  :: HTMLElement -> (DOMString -> (IO ()))
foreign import javascript unsafe "$1.accessKeyLabel" js_get_accessKeyLabel
  :: HTMLElement -> (IO DOMString)
foreign import javascript unsafe "$1.draggable" js_get_draggable
  :: HTMLElement -> (IO Bool)
foreign import javascript unsafe "$1.draggable = $2" js_set_draggable
  :: HTMLElement -> (Bool -> (IO ()))
foreign import javascript unsafe "$1.contentEditable" js_get_contentEditable
  :: HTMLElement -> (IO DOMString)
foreign import javascript unsafe "$1.contentEditable = $2" js_set_contentEditable
  :: HTMLElement -> (DOMString -> (IO ()))
foreign import javascript unsafe "$1.isContentEditable" js_get_isContentEditable
  :: HTMLElement -> (IO Bool)
foreign import javascript unsafe "$1.spellcheck" js_get_spellcheck
  :: HTMLElement -> (IO Bool)
foreign import javascript unsafe "$1.spellcheck = $2" js_set_spellcheck
  :: HTMLElement -> (Bool -> (IO ()))
foreign import javascript unsafe "$1.style" js_get_style
  :: HTMLElement -> (IO CSSStyleDeclaration)
foreign import javascript unsafe "$1.offsetParent" js_get_offsetParent
  :: HTMLElement -> (IO (Nullable ElementClass))
foreign import javascript unsafe "$1.offsetTop" js_get_offsetTop
  :: HTMLElement -> (IO Int32)
foreign import javascript unsafe "$1.offsetLeft" js_get_offsetLeft
  :: HTMLElement -> (IO Int32)
foreign import javascript unsafe "$1.offsetWidth" js_get_offsetWidth
  :: HTMLElement -> (IO Int32)
foreign import javascript unsafe "$1.offsetHeight" js_get_offsetHeight
  :: HTMLElement -> (IO Int32)
foreign import javascript unsafe "$1.onabort" js_get_onabort
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onabort = $2" js_set_onabort
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onblur" js_get_onblur
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onblur = $2" js_set_onblur
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onfocus" js_get_onfocus
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onfocus = $2" js_set_onfocus
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onauxclick" js_get_onauxclick
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onauxclick = $2" js_set_onauxclick
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.oncanplay" js_get_oncanplay
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.oncanplay = $2" js_set_oncanplay
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.oncanplaythrough" js_get_oncanplaythrough
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.oncanplaythrough = $2" js_set_oncanplaythrough
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onchange" js_get_onchange
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onchange = $2" js_set_onchange
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onclick" js_get_onclick
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onclick = $2" js_set_onclick
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onclose" js_get_onclose
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onclose = $2" js_set_onclose
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.oncontextmenu" js_get_oncontextmenu
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.oncontextmenu = $2" js_set_oncontextmenu
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ondblclick" js_get_ondblclick
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.ondblclick = $2" js_set_ondblclick
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ondrag" js_get_ondrag
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.ondrag = $2" js_set_ondrag
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ondragend" js_get_ondragend
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.ondragend = $2" js_set_ondragend
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ondragenter" js_get_ondragenter
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.ondragenter = $2" js_set_ondragenter
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ondragexit" js_get_ondragexit
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.ondragexit = $2" js_set_ondragexit
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ondragleave" js_get_ondragleave
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.ondragleave = $2" js_set_ondragleave
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ondragover" js_get_ondragover
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.ondragover = $2" js_set_ondragover
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ondragstart" js_get_ondragstart
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.ondragstart = $2" js_set_ondragstart
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ondrop" js_get_ondrop
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.ondrop = $2" js_set_ondrop
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ondurationchange" js_get_ondurationchange
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.ondurationchange = $2" js_set_ondurationchange
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onemptied" js_get_onemptied
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onemptied = $2" js_set_onemptied
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onended" js_get_onended
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onended = $2" js_set_onended
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.oninput" js_get_oninput
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.oninput = $2" js_set_oninput
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.oninvalid" js_get_oninvalid
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.oninvalid = $2" js_set_oninvalid
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onkeydown" js_get_onkeydown
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onkeydown = $2" js_set_onkeydown
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onkeypress" js_get_onkeypress
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onkeypress = $2" js_set_onkeypress
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onkeyup" js_get_onkeyup
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onkeyup = $2" js_set_onkeyup
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onload" js_get_onload
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onload = $2" js_set_onload
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onloadeddata" js_get_onloadeddata
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onloadeddata = $2" js_set_onloadeddata
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onloadedmetadata" js_get_onloadedmetadata
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onloadedmetadata = $2" js_set_onloadedmetadata
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onloadend" js_get_onloadend
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onloadend = $2" js_set_onloadend
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onloadstart" js_get_onloadstart
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onloadstart = $2" js_set_onloadstart
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onmousedown" js_get_onmousedown
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onmousedown = $2" js_set_onmousedown
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onmouseenter" js_get_onmouseenter
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onmouseenter = $2" js_set_onmouseenter
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onmouseleave" js_get_onmouseleave
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onmouseleave = $2" js_set_onmouseleave
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onmousemove" js_get_onmousemove
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onmousemove = $2" js_set_onmousemove
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onmouseout" js_get_onmouseout
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onmouseout = $2" js_set_onmouseout
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onmouseover" js_get_onmouseover
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onmouseover = $2" js_set_onmouseover
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onmouseup" js_get_onmouseup
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onmouseup = $2" js_set_onmouseup
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onwheel" js_get_onwheel
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onwheel = $2" js_set_onwheel
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onpause" js_get_onpause
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onpause = $2" js_set_onpause
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onplay" js_get_onplay
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onplay = $2" js_set_onplay
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onplaying" js_get_onplaying
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onplaying = $2" js_set_onplaying
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onprogress" js_get_onprogress
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onprogress = $2" js_set_onprogress
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onratechange" js_get_onratechange
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onratechange = $2" js_set_onratechange
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onreset" js_get_onreset
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onreset = $2" js_set_onreset
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onresize" js_get_onresize
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onresize = $2" js_set_onresize
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onscroll" js_get_onscroll
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onscroll = $2" js_set_onscroll
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onseeked" js_get_onseeked
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onseeked = $2" js_set_onseeked
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onseeking" js_get_onseeking
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onseeking = $2" js_set_onseeking
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onselect" js_get_onselect
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onselect = $2" js_set_onselect
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onshow" js_get_onshow
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onshow = $2" js_set_onshow
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onstalled" js_get_onstalled
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onstalled = $2" js_set_onstalled
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onsubmit" js_get_onsubmit
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onsubmit = $2" js_set_onsubmit
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onsuspend" js_get_onsuspend
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onsuspend = $2" js_set_onsuspend
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ontimeupdate" js_get_ontimeupdate
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.ontimeupdate = $2" js_set_ontimeupdate
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onvolumechange" js_get_onvolumechange
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onvolumechange = $2" js_set_onvolumechange
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onwaiting" js_get_onwaiting
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onwaiting = $2" js_set_onwaiting
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onselectstart" js_get_onselectstart
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onselectstart = $2" js_set_onselectstart
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ontoggle" js_get_ontoggle
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.ontoggle = $2" js_set_ontoggle
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onpointercancel" js_get_onpointercancel
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onpointercancel = $2" js_set_onpointercancel
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onpointerdown" js_get_onpointerdown
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onpointerdown = $2" js_set_onpointerdown
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onpointerup" js_get_onpointerup
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onpointerup = $2" js_set_onpointerup
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onpointermove" js_get_onpointermove
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onpointermove = $2" js_set_onpointermove
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onpointerout" js_get_onpointerout
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onpointerout = $2" js_set_onpointerout
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onpointerover" js_get_onpointerover
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onpointerover = $2" js_set_onpointerover
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onpointerenter" js_get_onpointerenter
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onpointerenter = $2" js_set_onpointerenter
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onpointerleave" js_get_onpointerleave
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onpointerleave = $2" js_set_onpointerleave
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ongotpointercapture" js_get_ongotpointercapture
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.ongotpointercapture = $2" js_set_ongotpointercapture
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onlostpointercapture" js_get_onlostpointercapture
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onlostpointercapture = $2" js_set_onlostpointercapture
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onanimationcancel" js_get_onanimationcancel
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onanimationcancel = $2" js_set_onanimationcancel
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onanimationend" js_get_onanimationend
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onanimationend = $2" js_set_onanimationend
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onanimationiteration" js_get_onanimationiteration
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onanimationiteration = $2" js_set_onanimationiteration
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onanimationstart" js_get_onanimationstart
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onanimationstart = $2" js_set_onanimationstart
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ontransitioncancel" js_get_ontransitioncancel
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.ontransitioncancel = $2" js_set_ontransitioncancel
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ontransitionend" js_get_ontransitionend
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.ontransitionend = $2" js_set_ontransitionend
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ontransitionrun" js_get_ontransitionrun
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.ontransitionrun = $2" js_set_ontransitionrun
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ontransitionstart" js_get_ontransitionstart
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.ontransitionstart = $2" js_set_ontransitionstart
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onwebkitanimationend" js_get_onwebkitanimationend
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onwebkitanimationend = $2" js_set_onwebkitanimationend
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onwebkitanimationiteration" js_get_onwebkitanimationiteration
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onwebkitanimationiteration = $2" js_set_onwebkitanimationiteration
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onwebkitanimationstart" js_get_onwebkitanimationstart
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onwebkitanimationstart = $2" js_set_onwebkitanimationstart
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onwebkittransitionend" js_get_onwebkittransitionend
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onwebkittransitionend = $2" js_set_onwebkittransitionend
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.oncopy" js_get_oncopy
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.oncopy = $2" js_set_oncopy
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.oncut" js_get_oncut
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.oncut = $2" js_set_oncut
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onpaste" js_get_onpaste
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onpaste = $2" js_set_onpaste
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ontouchstart" js_get_ontouchstart
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.ontouchstart = $2" js_set_ontouchstart
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ontouchend" js_get_ontouchend
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.ontouchend = $2" js_set_ontouchend
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ontouchmove" js_get_ontouchmove
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.ontouchmove = $2" js_set_ontouchmove
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ontouchcancel" js_get_ontouchcancel
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.ontouchcancel = $2" js_set_ontouchcancel
  :: HTMLElement -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onerror" js_get_onerror
  :: HTMLElement -> (IO EventHandler)
foreign import javascript unsafe "$1.onerror = $2" js_set_onerror
  :: HTMLElement -> (EventHandler -> (IO ()))
