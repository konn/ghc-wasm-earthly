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
module GHC.Wasm.Web.Generated.Window (
        Window, WindowClass, js_fun_close__undefined,
        js_fun_stop__undefined, js_fun_focus__undefined,
        js_fun_blur__undefined,
        js_fun_open_nullable_DOMString_nullable_DOMString_nullable_DOMString_nullable_WindowProxy,
        js_fun_alert__undefined, js_fun_alert_DOMString_undefined,
        js_fun_confirm_nullable_DOMString_boolean,
        js_fun_prompt_nullable_DOMString_nullable_DOMString_nullable_DOMString,
        js_fun_print__undefined,
        js_fun_postMessage_any_DOMString_nullable_sequence_object_undefined,
        js_fun_showOpenFilePicker_nullable_OpenFilePickerOptions_Promise_sequence_FileSystemFileHandle,
        js_fun_showSaveFilePicker_nullable_SaveFilePickerOptions_Promise_FileSystemFileHandle,
        js_fun_showDirectoryPicker_nullable_DirectoryPickerOptions_Promise_FileSystemDirectoryHandle,
        js_fun_queryLocalFonts_nullable_QueryOptions_Promise_sequence_FontData,
        js_fun_captureEvents__undefined, js_fun_releaseEvents__undefined,
        js_fun_getSelection__nullable_Selection,
        js_fun_getComputedStyle_Element_nullable_DOMString_nullable_CSSStyleDeclaration,
        js_fun_matchMedia_DOMString_nullable_MediaQueryList,
        js_fun_moveTo_long_long_undefined,
        js_fun_moveBy_long_long_undefined,
        js_fun_resizeTo_long_long_undefined,
        js_fun_resizeBy_long_long_undefined,
        js_fun_scroll_double_double_undefined,
        js_fun_scroll_nullable_ScrollToOptions_undefined,
        js_fun_scrollTo_double_double_undefined,
        js_fun_scrollTo_nullable_ScrollToOptions_undefined,
        js_fun_scrollBy_double_double_undefined,
        js_fun_scrollBy_nullable_ScrollToOptions_undefined,
        js_fun_requestIdleCallback_IdleRequestCallback_nullable_IdleRequestOptions_long,
        js_fun_cancelIdleCallback_long_undefined,
        js_fun_requestAnimationFrame_FrameRequestCallback_long,
        js_fun_cancelAnimationFrame_long_undefined,
        js_fun_btoa_DOMString_DOMString, js_fun_atob_DOMString_DOMString,
        js_fun_setTimeout_Function_nullable_long_any_long,
        js_fun_setTimeout_DOMString_nullable_long_any_long,
        js_fun_clearTimeout_nullable_long_undefined,
        js_fun_setInterval_Function_nullable_long_any_long,
        js_fun_setInterval_DOMString_nullable_long_any_long,
        js_fun_clearInterval_nullable_long_undefined,
        js_fun_createImageBitmap_ImageBitmapSource_nullable_ImageBitmapOptions_Promise_ImageBitmap,
        js_fun_createImageBitmap_ImageBitmapSource_long_long_long_long_nullable_ImageBitmapOptions_Promise_ImageBitmap,
        js_fun_fetch_RequestInfo_nullable_RequestInit_Promise_Response,
        js_fun_queueMicrotask_VoidFunction_undefined, js_get_window,
        js_get_self, js_get_document, js_get_name, js_set_name,
        js_get_location, js_get_history, js_get_customElements,
        js_get_locationbar, js_get_menubar, js_get_personalbar,
        js_get_scrollbars, js_get_statusbar, js_get_toolbar, js_get_status,
        js_set_status, js_get_closed, js_get_event, js_get_frames,
        js_get_length, js_get_top, js_get_opener, js_set_opener,
        js_get_parent, js_get_frameElement, js_get_navigator,
        js_get_external, js_get_applicationCache, js_get_onappinstalled,
        js_set_onappinstalled, js_get_screen, js_get_visualViewport,
        js_get_innerWidth, js_set_innerWidth, js_get_innerHeight,
        js_set_innerHeight, js_get_scrollX, js_get_pageXOffset,
        js_get_scrollY, js_get_pageYOffset, js_get_screenX, js_set_screenX,
        js_get_screenY, js_set_screenY, js_get_outerWidth,
        js_set_outerWidth, js_get_outerHeight, js_set_outerHeight,
        js_get_devicePixelRatio, js_get_orientation,
        js_get_onorientationchange, js_set_onorientationchange,
        js_get_onvrdisplayconnect, js_set_onvrdisplayconnect,
        js_get_onvrdisplaydisconnect, js_set_onvrdisplaydisconnect,
        js_get_onvrdisplayactivate, js_set_onvrdisplayactivate,
        js_get_onvrdisplaydeactivate, js_set_onvrdisplaydeactivate,
        js_get_onvrdisplaypresentchange, js_set_onvrdisplaypresentchange,
        js_get_paintWorklet, js_get_onabort, js_set_onabort, js_get_onblur,
        js_set_onblur, js_get_onfocus, js_set_onfocus, js_get_onauxclick,
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
        js_set_onwebkittransitionend, js_get_onafterprint,
        js_set_onafterprint, js_get_onbeforeprint, js_set_onbeforeprint,
        js_get_onbeforeunload, js_set_onbeforeunload, js_get_onhashchange,
        js_set_onhashchange, js_get_onlanguagechange,
        js_set_onlanguagechange, js_get_onmessage, js_set_onmessage,
        js_get_onmessageerror, js_set_onmessageerror, js_get_onoffline,
        js_set_onoffline, js_get_ononline, js_set_ononline,
        js_get_onpagehide, js_set_onpagehide, js_get_onpageshow,
        js_set_onpageshow, js_get_onpopstate, js_set_onpopstate,
        js_get_onstorage, js_set_onstorage, js_get_onunload,
        js_set_onunload, js_get_sessionStorage, js_get_localStorage,
        js_get_crypto, js_get_u2f, js_get_speechSynthesis,
        js_get_ontouchstart, js_set_ontouchstart, js_get_ontouchend,
        js_set_ontouchend, js_get_ontouchmove, js_set_ontouchmove,
        js_get_ontouchcancel, js_set_ontouchcancel, js_get_onerror,
        js_set_onerror, js_get_origin, js_get_scheduler,
        js_get_isSecureContext, js_get_indexedDB, js_get_caches,
        js_get_performance
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.ApplicationCache.Core
import GHC.Wasm.Web.Generated.BarProp.Core
import GHC.Wasm.Web.Generated.CSSStyleDeclaration.Core
import GHC.Wasm.Web.Generated.CacheStorage.Core
import GHC.Wasm.Web.Generated.Crypto.Core
import GHC.Wasm.Web.Generated.CustomElementRegistry.Core
import GHC.Wasm.Web.Generated.DirectoryPickerOptions.Core
import GHC.Wasm.Web.Generated.Document.Core
import GHC.Wasm.Web.Generated.Element.Core
import GHC.Wasm.Web.Generated.EventHandler.Core
import GHC.Wasm.Web.Generated.EventTarget.Core
import GHC.Wasm.Web.Generated.External.Core
import GHC.Wasm.Web.Generated.FileSystemDirectoryHandle.Core
import GHC.Wasm.Web.Generated.FileSystemFileHandle.Core
import GHC.Wasm.Web.Generated.FontData.Core
import GHC.Wasm.Web.Generated.FrameRequestCallback.Core
import GHC.Wasm.Web.Generated.Function.Core
import GHC.Wasm.Web.Generated.History.Core
import GHC.Wasm.Web.Generated.IDBFactory.Core
import GHC.Wasm.Web.Generated.IdleRequestCallback.Core
import GHC.Wasm.Web.Generated.IdleRequestOptions.Core
import GHC.Wasm.Web.Generated.ImageBitmap.Core
import GHC.Wasm.Web.Generated.ImageBitmapOptions.Core
import GHC.Wasm.Web.Generated.ImageBitmapSource.Core
import GHC.Wasm.Web.Generated.Location.Core
import GHC.Wasm.Web.Generated.MediaQueryList.Core
import GHC.Wasm.Web.Generated.Navigator.Core
import GHC.Wasm.Web.Generated.OnBeforeUnloadEventHandler.Core
import GHC.Wasm.Web.Generated.OnErrorEventHandler.Core
import GHC.Wasm.Web.Generated.OpenFilePickerOptions.Core
import GHC.Wasm.Web.Generated.Performance.Core
import GHC.Wasm.Web.Generated.QueryOptions.Core
import GHC.Wasm.Web.Generated.RequestInfo.Core
import GHC.Wasm.Web.Generated.RequestInit.Core
import GHC.Wasm.Web.Generated.Response.Core
import GHC.Wasm.Web.Generated.SaveFilePickerOptions.Core
import GHC.Wasm.Web.Generated.Scheduler.Core
import GHC.Wasm.Web.Generated.Screen.Core
import GHC.Wasm.Web.Generated.ScrollToOptions.Core
import GHC.Wasm.Web.Generated.Selection.Core
import GHC.Wasm.Web.Generated.SpeechSynthesis.Core
import GHC.Wasm.Web.Generated.Storage.Core
import GHC.Wasm.Web.Generated.U2F.Core
import GHC.Wasm.Web.Generated.VisualViewport.Core
import GHC.Wasm.Web.Generated.VoidFunction.Core
import GHC.Wasm.Web.Generated.Window.Core
import GHC.Wasm.Web.Generated.WindowProxy.Core
import GHC.Wasm.Web.Generated.Worklet.Core
import GHC.Wasm.Web.Types
foreign import javascript unsafe "$1.close()" js_fun_close__undefined
  :: Window -> (IO ())
foreign import javascript unsafe "$1.stop()" js_fun_stop__undefined
  :: Window -> (IO ())
foreign import javascript unsafe "$1.focus()" js_fun_focus__undefined
  :: Window -> (IO ())
foreign import javascript unsafe "$1.blur()" js_fun_blur__undefined
  :: Window -> (IO ())
foreign import javascript unsafe "$1.open($2,$3,$4)" js_fun_open_nullable_DOMString_nullable_DOMString_nullable_DOMString_nullable_WindowProxy
  :: Window
     -> (Nullable DOMStringClass
         -> (Nullable DOMStringClass
             -> (Nullable DOMStringClass -> (IO (Nullable WindowProxyClass)))))
foreign import javascript unsafe "$1.alert()" js_fun_alert__undefined
  :: Window -> (IO ())
foreign import javascript unsafe "$1.alert($2)" js_fun_alert_DOMString_undefined
  :: Window -> (DOMString -> (IO ()))
foreign import javascript unsafe "$1.confirm($2)" js_fun_confirm_nullable_DOMString_boolean
  :: Window -> (Nullable DOMStringClass -> (IO Bool))
foreign import javascript unsafe "$1.prompt($2,$3)" js_fun_prompt_nullable_DOMString_nullable_DOMString_nullable_DOMString
  :: Window
     -> (Nullable DOMStringClass
         -> (Nullable DOMStringClass -> (IO (Nullable DOMStringClass))))
foreign import javascript unsafe "$1.print()" js_fun_print__undefined
  :: Window -> (IO ())
foreign import javascript unsafe "$1.postMessage($2,$3,$4)" js_fun_postMessage_any_DOMString_nullable_sequence_object_undefined
  :: Window
     -> (JSAny
         -> (DOMString -> (Nullable (SequenceClass AnyClass) -> (IO ()))))
foreign import javascript safe "$1.showOpenFilePicker($2)" js_fun_showOpenFilePicker_nullable_OpenFilePickerOptions_Promise_sequence_FileSystemFileHandle
  :: Window
     -> (Nullable OpenFilePickerOptionsClass
         -> (IO (Promise (SequenceClass FileSystemFileHandleClass))))
foreign import javascript safe "$1.showSaveFilePicker($2)" js_fun_showSaveFilePicker_nullable_SaveFilePickerOptions_Promise_FileSystemFileHandle
  :: Window
     -> (Nullable SaveFilePickerOptionsClass
         -> (IO (Promise FileSystemFileHandleClass)))
foreign import javascript safe "$1.showDirectoryPicker($2)" js_fun_showDirectoryPicker_nullable_DirectoryPickerOptions_Promise_FileSystemDirectoryHandle
  :: Window
     -> (Nullable DirectoryPickerOptionsClass
         -> (IO (Promise FileSystemDirectoryHandleClass)))
foreign import javascript safe "$1.queryLocalFonts($2)" js_fun_queryLocalFonts_nullable_QueryOptions_Promise_sequence_FontData
  :: Window
     -> (Nullable QueryOptionsClass
         -> (IO (Promise (SequenceClass FontDataClass))))
foreign import javascript unsafe "$1.captureEvents()" js_fun_captureEvents__undefined
  :: Window -> (IO ())
foreign import javascript unsafe "$1.releaseEvents()" js_fun_releaseEvents__undefined
  :: Window -> (IO ())
foreign import javascript unsafe "$1.getSelection()" js_fun_getSelection__nullable_Selection
  :: Window -> (IO (Nullable SelectionClass))
foreign import javascript unsafe "$1.getComputedStyle($2,$3)" js_fun_getComputedStyle_Element_nullable_DOMString_nullable_CSSStyleDeclaration
  :: Window
     -> (Element
         -> (Nullable DOMStringClass
             -> (IO (Nullable CSSStyleDeclarationClass))))
foreign import javascript unsafe "$1.matchMedia($2)" js_fun_matchMedia_DOMString_nullable_MediaQueryList
  :: Window -> (DOMString -> (IO (Nullable MediaQueryListClass)))
foreign import javascript unsafe "$1.moveTo($2,$3)" js_fun_moveTo_long_long_undefined
  :: Window -> (Int32 -> (Int32 -> (IO ())))
foreign import javascript unsafe "$1.moveBy($2,$3)" js_fun_moveBy_long_long_undefined
  :: Window -> (Int32 -> (Int32 -> (IO ())))
foreign import javascript unsafe "$1.resizeTo($2,$3)" js_fun_resizeTo_long_long_undefined
  :: Window -> (Int32 -> (Int32 -> (IO ())))
foreign import javascript unsafe "$1.resizeBy($2,$3)" js_fun_resizeBy_long_long_undefined
  :: Window -> (Int32 -> (Int32 -> (IO ())))
foreign import javascript unsafe "$1.scroll($2,$3)" js_fun_scroll_double_double_undefined
  :: Window -> (Double -> (Double -> (IO ())))
foreign import javascript unsafe "$1.scroll($2)" js_fun_scroll_nullable_ScrollToOptions_undefined
  :: Window -> (Nullable ScrollToOptionsClass -> (IO ()))
foreign import javascript unsafe "$1.scrollTo($2,$3)" js_fun_scrollTo_double_double_undefined
  :: Window -> (Double -> (Double -> (IO ())))
foreign import javascript unsafe "$1.scrollTo($2)" js_fun_scrollTo_nullable_ScrollToOptions_undefined
  :: Window -> (Nullable ScrollToOptionsClass -> (IO ()))
foreign import javascript unsafe "$1.scrollBy($2,$3)" js_fun_scrollBy_double_double_undefined
  :: Window -> (Double -> (Double -> (IO ())))
foreign import javascript unsafe "$1.scrollBy($2)" js_fun_scrollBy_nullable_ScrollToOptions_undefined
  :: Window -> (Nullable ScrollToOptionsClass -> (IO ()))
foreign import javascript unsafe "$1.requestIdleCallback($2,$3)" js_fun_requestIdleCallback_IdleRequestCallback_nullable_IdleRequestOptions_long
  :: Window
     -> (IdleRequestCallback
         -> (Nullable IdleRequestOptionsClass -> (IO Word32)))
foreign import javascript unsafe "$1.cancelIdleCallback($2)" js_fun_cancelIdleCallback_long_undefined
  :: Window -> (Word32 -> (IO ()))
foreign import javascript unsafe "$1.requestAnimationFrame($2)" js_fun_requestAnimationFrame_FrameRequestCallback_long
  :: Window -> (FrameRequestCallback -> (IO Int32))
foreign import javascript unsafe "$1.cancelAnimationFrame($2)" js_fun_cancelAnimationFrame_long_undefined
  :: Window -> (Int32 -> (IO ()))
foreign import javascript unsafe "$1.btoa($2)" js_fun_btoa_DOMString_DOMString
  :: Window -> (DOMString -> (IO DOMString))
foreign import javascript unsafe "$1.atob($2)" js_fun_atob_DOMString_DOMString
  :: Window -> (DOMString -> (IO DOMString))
foreign import javascript unsafe "$1.setTimeout($2,$3,... $4)" js_fun_setTimeout_Function_nullable_long_any_long
  :: Window
     -> (Function
         -> (Nullable (JSPrimClass Int32)
             -> (FrozenArray AnyClass -> (IO Int32))))
foreign import javascript unsafe "$1.setTimeout($2,$3,... $4)" js_fun_setTimeout_DOMString_nullable_long_any_long
  :: Window
     -> (DOMString
         -> (Nullable (JSPrimClass Int32)
             -> (FrozenArray AnyClass -> (IO Int32))))
foreign import javascript unsafe "$1.clearTimeout($2)" js_fun_clearTimeout_nullable_long_undefined
  :: Window -> (Nullable (JSPrimClass Int32) -> (IO ()))
foreign import javascript unsafe "$1.setInterval($2,$3,... $4)" js_fun_setInterval_Function_nullable_long_any_long
  :: Window
     -> (Function
         -> (Nullable (JSPrimClass Int32)
             -> (FrozenArray AnyClass -> (IO Int32))))
foreign import javascript unsafe "$1.setInterval($2,$3,... $4)" js_fun_setInterval_DOMString_nullable_long_any_long
  :: Window
     -> (DOMString
         -> (Nullable (JSPrimClass Int32)
             -> (FrozenArray AnyClass -> (IO Int32))))
foreign import javascript unsafe "$1.clearInterval($2)" js_fun_clearInterval_nullable_long_undefined
  :: Window -> (Nullable (JSPrimClass Int32) -> (IO ()))
foreign import javascript safe "$1.createImageBitmap($2,$3)" js_fun_createImageBitmap_ImageBitmapSource_nullable_ImageBitmapOptions_Promise_ImageBitmap
  :: Window
     -> (ImageBitmapSource
         -> (Nullable ImageBitmapOptionsClass
             -> (IO (Promise ImageBitmapClass))))
foreign import javascript safe "$1.createImageBitmap($2,$3,$4,$5,$6,$7)" js_fun_createImageBitmap_ImageBitmapSource_long_long_long_long_nullable_ImageBitmapOptions_Promise_ImageBitmap
  :: Window
     -> (ImageBitmapSource
         -> (Int32
             -> (Int32
                 -> (Int32
                     -> (Int32
                         -> (Nullable ImageBitmapOptionsClass
                             -> (IO (Promise ImageBitmapClass))))))))
foreign import javascript safe "$1.fetch($2,$3)" js_fun_fetch_RequestInfo_nullable_RequestInit_Promise_Response
  :: Window
     -> (RequestInfo
         -> (Nullable RequestInitClass -> (IO (Promise ResponseClass))))
foreign import javascript unsafe "$1.queueMicrotask($2)" js_fun_queueMicrotask_VoidFunction_undefined
  :: Window -> (VoidFunction -> (IO ()))
foreign import javascript unsafe "$1.window" js_get_window
  :: Window -> (IO Window)
foreign import javascript unsafe "$1.self" js_get_self
  :: Window -> (IO Window)
foreign import javascript unsafe "$1.document" js_get_document
  :: Window -> (IO (Nullable DocumentClass))
foreign import javascript unsafe "$1.name" js_get_name
  :: Window -> (IO DOMString)
foreign import javascript unsafe "$1.name = $2" js_set_name
  :: Window -> (DOMString -> (IO ()))
foreign import javascript unsafe "$1.location" js_get_location
  :: Window -> (IO Location)
foreign import javascript unsafe "$1.history" js_get_history
  :: Window -> (IO History)
foreign import javascript unsafe "$1.customElements" js_get_customElements
  :: Window -> (IO CustomElementRegistry)
foreign import javascript unsafe "$1.locationbar" js_get_locationbar
  :: Window -> (IO BarProp)
foreign import javascript unsafe "$1.menubar" js_get_menubar
  :: Window -> (IO BarProp)
foreign import javascript unsafe "$1.personalbar" js_get_personalbar
  :: Window -> (IO BarProp)
foreign import javascript unsafe "$1.scrollbars" js_get_scrollbars
  :: Window -> (IO BarProp)
foreign import javascript unsafe "$1.statusbar" js_get_statusbar
  :: Window -> (IO BarProp)
foreign import javascript unsafe "$1.toolbar" js_get_toolbar
  :: Window -> (IO BarProp)
foreign import javascript unsafe "$1.status" js_get_status
  :: Window -> (IO DOMString)
foreign import javascript unsafe "$1.status = $2" js_set_status
  :: Window -> (DOMString -> (IO ()))
foreign import javascript unsafe "$1.closed" js_get_closed
  :: Window -> (IO Bool)
foreign import javascript unsafe "$1.event" js_get_event
  :: Window -> (IO JSAny)
foreign import javascript unsafe "$1.frames" js_get_frames
  :: Window -> (IO WindowProxy)
foreign import javascript unsafe "$1.length" js_get_length
  :: Window -> (IO Word32)
foreign import javascript unsafe "$1.top" js_get_top
  :: Window -> (IO (Nullable WindowProxyClass))
foreign import javascript unsafe "$1.opener" js_get_opener
  :: Window -> (IO JSAny)
foreign import javascript unsafe "$1.opener = $2" js_set_opener
  :: Window -> (JSAny -> (IO ()))
foreign import javascript unsafe "$1.parent" js_get_parent
  :: Window -> (IO (Nullable WindowProxyClass))
foreign import javascript unsafe "$1.frameElement" js_get_frameElement
  :: Window -> (IO (Nullable ElementClass))
foreign import javascript unsafe "$1.navigator" js_get_navigator
  :: Window -> (IO Navigator)
foreign import javascript unsafe "$1.external" js_get_external
  :: Window -> (IO External)
foreign import javascript unsafe "$1.applicationCache" js_get_applicationCache
  :: Window -> (IO ApplicationCache)
foreign import javascript unsafe "$1.onappinstalled" js_get_onappinstalled
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.onappinstalled = $2" js_set_onappinstalled
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.screen" js_get_screen
  :: Window -> (IO Screen)
foreign import javascript unsafe "$1.visualViewport" js_get_visualViewport
  :: Window -> (IO (Nullable VisualViewportClass))
foreign import javascript unsafe "$1.innerWidth" js_get_innerWidth
  :: Window -> (IO JSAny)
foreign import javascript unsafe "$1.innerWidth = $2" js_set_innerWidth
  :: Window -> (JSAny -> (IO ()))
foreign import javascript unsafe "$1.innerHeight" js_get_innerHeight
  :: Window -> (IO JSAny)
foreign import javascript unsafe "$1.innerHeight = $2" js_set_innerHeight
  :: Window -> (JSAny -> (IO ()))
foreign import javascript unsafe "$1.scrollX" js_get_scrollX
  :: Window -> (IO Double)
foreign import javascript unsafe "$1.pageXOffset" js_get_pageXOffset
  :: Window -> (IO Double)
foreign import javascript unsafe "$1.scrollY" js_get_scrollY
  :: Window -> (IO Double)
foreign import javascript unsafe "$1.pageYOffset" js_get_pageYOffset
  :: Window -> (IO Double)
foreign import javascript unsafe "$1.screenX" js_get_screenX
  :: Window -> (IO JSAny)
foreign import javascript unsafe "$1.screenX = $2" js_set_screenX
  :: Window -> (JSAny -> (IO ()))
foreign import javascript unsafe "$1.screenY" js_get_screenY
  :: Window -> (IO JSAny)
foreign import javascript unsafe "$1.screenY = $2" js_set_screenY
  :: Window -> (JSAny -> (IO ()))
foreign import javascript unsafe "$1.outerWidth" js_get_outerWidth
  :: Window -> (IO JSAny)
foreign import javascript unsafe "$1.outerWidth = $2" js_set_outerWidth
  :: Window -> (JSAny -> (IO ()))
foreign import javascript unsafe "$1.outerHeight" js_get_outerHeight
  :: Window -> (IO JSAny)
foreign import javascript unsafe "$1.outerHeight = $2" js_set_outerHeight
  :: Window -> (JSAny -> (IO ()))
foreign import javascript unsafe "$1.devicePixelRatio" js_get_devicePixelRatio
  :: Window -> (IO Double)
foreign import javascript unsafe "$1.orientation" js_get_orientation
  :: Window -> (IO Int16)
foreign import javascript unsafe "$1.onorientationchange" js_get_onorientationchange
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.onorientationchange = $2" js_set_onorientationchange
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onvrdisplayconnect" js_get_onvrdisplayconnect
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.onvrdisplayconnect = $2" js_set_onvrdisplayconnect
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onvrdisplaydisconnect" js_get_onvrdisplaydisconnect
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.onvrdisplaydisconnect = $2" js_set_onvrdisplaydisconnect
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onvrdisplayactivate" js_get_onvrdisplayactivate
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.onvrdisplayactivate = $2" js_set_onvrdisplayactivate
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onvrdisplaydeactivate" js_get_onvrdisplaydeactivate
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.onvrdisplaydeactivate = $2" js_set_onvrdisplaydeactivate
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onvrdisplaypresentchange" js_get_onvrdisplaypresentchange
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.onvrdisplaypresentchange = $2" js_set_onvrdisplaypresentchange
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.paintWorklet" js_get_paintWorklet
  :: Window -> (IO Worklet)
foreign import javascript unsafe "$1.onabort" js_get_onabort
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.onabort = $2" js_set_onabort
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onblur" js_get_onblur
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.onblur = $2" js_set_onblur
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onfocus" js_get_onfocus
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.onfocus = $2" js_set_onfocus
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onauxclick" js_get_onauxclick
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.onauxclick = $2" js_set_onauxclick
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.oncanplay" js_get_oncanplay
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.oncanplay = $2" js_set_oncanplay
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.oncanplaythrough" js_get_oncanplaythrough
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.oncanplaythrough = $2" js_set_oncanplaythrough
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onchange" js_get_onchange
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.onchange = $2" js_set_onchange
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onclick" js_get_onclick
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.onclick = $2" js_set_onclick
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onclose" js_get_onclose
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.onclose = $2" js_set_onclose
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.oncontextmenu" js_get_oncontextmenu
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.oncontextmenu = $2" js_set_oncontextmenu
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ondblclick" js_get_ondblclick
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.ondblclick = $2" js_set_ondblclick
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ondrag" js_get_ondrag
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.ondrag = $2" js_set_ondrag
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ondragend" js_get_ondragend
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.ondragend = $2" js_set_ondragend
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ondragenter" js_get_ondragenter
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.ondragenter = $2" js_set_ondragenter
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ondragexit" js_get_ondragexit
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.ondragexit = $2" js_set_ondragexit
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ondragleave" js_get_ondragleave
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.ondragleave = $2" js_set_ondragleave
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ondragover" js_get_ondragover
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.ondragover = $2" js_set_ondragover
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ondragstart" js_get_ondragstart
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.ondragstart = $2" js_set_ondragstart
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ondrop" js_get_ondrop
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.ondrop = $2" js_set_ondrop
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ondurationchange" js_get_ondurationchange
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.ondurationchange = $2" js_set_ondurationchange
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onemptied" js_get_onemptied
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.onemptied = $2" js_set_onemptied
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onended" js_get_onended
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.onended = $2" js_set_onended
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.oninput" js_get_oninput
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.oninput = $2" js_set_oninput
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.oninvalid" js_get_oninvalid
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.oninvalid = $2" js_set_oninvalid
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onkeydown" js_get_onkeydown
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.onkeydown = $2" js_set_onkeydown
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onkeypress" js_get_onkeypress
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.onkeypress = $2" js_set_onkeypress
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onkeyup" js_get_onkeyup
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.onkeyup = $2" js_set_onkeyup
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onload" js_get_onload
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.onload = $2" js_set_onload
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onloadeddata" js_get_onloadeddata
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.onloadeddata = $2" js_set_onloadeddata
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onloadedmetadata" js_get_onloadedmetadata
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.onloadedmetadata = $2" js_set_onloadedmetadata
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onloadend" js_get_onloadend
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.onloadend = $2" js_set_onloadend
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onloadstart" js_get_onloadstart
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.onloadstart = $2" js_set_onloadstart
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onmousedown" js_get_onmousedown
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.onmousedown = $2" js_set_onmousedown
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onmouseenter" js_get_onmouseenter
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.onmouseenter = $2" js_set_onmouseenter
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onmouseleave" js_get_onmouseleave
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.onmouseleave = $2" js_set_onmouseleave
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onmousemove" js_get_onmousemove
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.onmousemove = $2" js_set_onmousemove
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onmouseout" js_get_onmouseout
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.onmouseout = $2" js_set_onmouseout
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onmouseover" js_get_onmouseover
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.onmouseover = $2" js_set_onmouseover
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onmouseup" js_get_onmouseup
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.onmouseup = $2" js_set_onmouseup
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onwheel" js_get_onwheel
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.onwheel = $2" js_set_onwheel
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onpause" js_get_onpause
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.onpause = $2" js_set_onpause
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onplay" js_get_onplay
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.onplay = $2" js_set_onplay
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onplaying" js_get_onplaying
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.onplaying = $2" js_set_onplaying
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onprogress" js_get_onprogress
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.onprogress = $2" js_set_onprogress
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onratechange" js_get_onratechange
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.onratechange = $2" js_set_onratechange
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onreset" js_get_onreset
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.onreset = $2" js_set_onreset
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onresize" js_get_onresize
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.onresize = $2" js_set_onresize
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onscroll" js_get_onscroll
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.onscroll = $2" js_set_onscroll
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onseeked" js_get_onseeked
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.onseeked = $2" js_set_onseeked
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onseeking" js_get_onseeking
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.onseeking = $2" js_set_onseeking
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onselect" js_get_onselect
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.onselect = $2" js_set_onselect
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onshow" js_get_onshow
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.onshow = $2" js_set_onshow
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onstalled" js_get_onstalled
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.onstalled = $2" js_set_onstalled
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onsubmit" js_get_onsubmit
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.onsubmit = $2" js_set_onsubmit
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onsuspend" js_get_onsuspend
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.onsuspend = $2" js_set_onsuspend
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ontimeupdate" js_get_ontimeupdate
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.ontimeupdate = $2" js_set_ontimeupdate
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onvolumechange" js_get_onvolumechange
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.onvolumechange = $2" js_set_onvolumechange
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onwaiting" js_get_onwaiting
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.onwaiting = $2" js_set_onwaiting
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onselectstart" js_get_onselectstart
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.onselectstart = $2" js_set_onselectstart
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ontoggle" js_get_ontoggle
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.ontoggle = $2" js_set_ontoggle
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onpointercancel" js_get_onpointercancel
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.onpointercancel = $2" js_set_onpointercancel
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onpointerdown" js_get_onpointerdown
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.onpointerdown = $2" js_set_onpointerdown
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onpointerup" js_get_onpointerup
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.onpointerup = $2" js_set_onpointerup
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onpointermove" js_get_onpointermove
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.onpointermove = $2" js_set_onpointermove
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onpointerout" js_get_onpointerout
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.onpointerout = $2" js_set_onpointerout
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onpointerover" js_get_onpointerover
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.onpointerover = $2" js_set_onpointerover
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onpointerenter" js_get_onpointerenter
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.onpointerenter = $2" js_set_onpointerenter
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onpointerleave" js_get_onpointerleave
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.onpointerleave = $2" js_set_onpointerleave
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ongotpointercapture" js_get_ongotpointercapture
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.ongotpointercapture = $2" js_set_ongotpointercapture
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onlostpointercapture" js_get_onlostpointercapture
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.onlostpointercapture = $2" js_set_onlostpointercapture
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onanimationcancel" js_get_onanimationcancel
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.onanimationcancel = $2" js_set_onanimationcancel
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onanimationend" js_get_onanimationend
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.onanimationend = $2" js_set_onanimationend
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onanimationiteration" js_get_onanimationiteration
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.onanimationiteration = $2" js_set_onanimationiteration
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onanimationstart" js_get_onanimationstart
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.onanimationstart = $2" js_set_onanimationstart
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ontransitioncancel" js_get_ontransitioncancel
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.ontransitioncancel = $2" js_set_ontransitioncancel
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ontransitionend" js_get_ontransitionend
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.ontransitionend = $2" js_set_ontransitionend
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ontransitionrun" js_get_ontransitionrun
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.ontransitionrun = $2" js_set_ontransitionrun
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ontransitionstart" js_get_ontransitionstart
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.ontransitionstart = $2" js_set_ontransitionstart
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onwebkitanimationend" js_get_onwebkitanimationend
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.onwebkitanimationend = $2" js_set_onwebkitanimationend
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onwebkitanimationiteration" js_get_onwebkitanimationiteration
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.onwebkitanimationiteration = $2" js_set_onwebkitanimationiteration
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onwebkitanimationstart" js_get_onwebkitanimationstart
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.onwebkitanimationstart = $2" js_set_onwebkitanimationstart
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onwebkittransitionend" js_get_onwebkittransitionend
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.onwebkittransitionend = $2" js_set_onwebkittransitionend
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onafterprint" js_get_onafterprint
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.onafterprint = $2" js_set_onafterprint
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onbeforeprint" js_get_onbeforeprint
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.onbeforeprint = $2" js_set_onbeforeprint
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onbeforeunload" js_get_onbeforeunload
  :: Window -> (IO OnBeforeUnloadEventHandler)
foreign import javascript unsafe "$1.onbeforeunload = $2" js_set_onbeforeunload
  :: Window -> (OnBeforeUnloadEventHandler -> (IO ()))
foreign import javascript unsafe "$1.onhashchange" js_get_onhashchange
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.onhashchange = $2" js_set_onhashchange
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onlanguagechange" js_get_onlanguagechange
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.onlanguagechange = $2" js_set_onlanguagechange
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onmessage" js_get_onmessage
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.onmessage = $2" js_set_onmessage
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onmessageerror" js_get_onmessageerror
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.onmessageerror = $2" js_set_onmessageerror
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onoffline" js_get_onoffline
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.onoffline = $2" js_set_onoffline
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ononline" js_get_ononline
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.ononline = $2" js_set_ononline
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onpagehide" js_get_onpagehide
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.onpagehide = $2" js_set_onpagehide
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onpageshow" js_get_onpageshow
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.onpageshow = $2" js_set_onpageshow
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onpopstate" js_get_onpopstate
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.onpopstate = $2" js_set_onpopstate
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onstorage" js_get_onstorage
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.onstorage = $2" js_set_onstorage
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onunload" js_get_onunload
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.onunload = $2" js_set_onunload
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.sessionStorage" js_get_sessionStorage
  :: Window -> (IO (Nullable StorageClass))
foreign import javascript unsafe "$1.localStorage" js_get_localStorage
  :: Window -> (IO (Nullable StorageClass))
foreign import javascript unsafe "$1.crypto" js_get_crypto
  :: Window -> (IO Crypto)
foreign import javascript unsafe "$1.u2f" js_get_u2f
  :: Window -> (IO U2F)
foreign import javascript unsafe "$1.speechSynthesis" js_get_speechSynthesis
  :: Window -> (IO SpeechSynthesis)
foreign import javascript unsafe "$1.ontouchstart" js_get_ontouchstart
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.ontouchstart = $2" js_set_ontouchstart
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ontouchend" js_get_ontouchend
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.ontouchend = $2" js_set_ontouchend
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ontouchmove" js_get_ontouchmove
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.ontouchmove = $2" js_set_ontouchmove
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.ontouchcancel" js_get_ontouchcancel
  :: Window -> (IO EventHandler)
foreign import javascript unsafe "$1.ontouchcancel = $2" js_set_ontouchcancel
  :: Window -> (EventHandler -> (IO ()))
foreign import javascript unsafe "$1.onerror" js_get_onerror
  :: Window -> (IO OnErrorEventHandler)
foreign import javascript unsafe "$1.onerror = $2" js_set_onerror
  :: Window -> (OnErrorEventHandler -> (IO ()))
foreign import javascript unsafe "$1.origin" js_get_origin
  :: Window -> (IO USVString)
foreign import javascript unsafe "$1.scheduler" js_get_scheduler
  :: Window -> (IO Scheduler)
foreign import javascript unsafe "$1.isSecureContext" js_get_isSecureContext
  :: Window -> (IO Bool)
foreign import javascript unsafe "$1.indexedDB" js_get_indexedDB
  :: Window -> (IO (Nullable IDBFactoryClass))
foreign import javascript unsafe "$1.caches" js_get_caches
  :: Window -> (IO CacheStorage)
foreign import javascript unsafe "$1.performance" js_get_performance
  :: Window -> (IO (Nullable PerformanceClass))
