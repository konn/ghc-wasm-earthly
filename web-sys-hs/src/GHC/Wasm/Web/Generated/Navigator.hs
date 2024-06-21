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
module GHC.Wasm.Web.Generated.Navigator (
        Navigator, NavigatorClass,
        js_fun_getBattery__Promise_BatteryManager,
        js_fun_vibrate_long_boolean, js_fun_vibrate_sequence_long_boolean,
        js_fun_getGamepads__sequence_nullable_Gamepad,
        js_fun_requestGamepadServiceTest__GamepadServiceTest,
        js_fun_getVRDisplays__Promise_sequence_VRDisplay,
        js_fun_requestVRPresentation_VRDisplay_undefined,
        js_fun_requestVRServiceTest__VRServiceTest,
        js_fun_requestMIDIAccess_nullable_MIDIOptions_Promise_MIDIAccess,
        js_fun_sendBeacon_DOMString_nullable_nullable_BodyInit_boolean,
        js_fun_requestMediaKeySystemAccess_DOMString_sequence_MediaKeySystemConfiguration_Promise_MediaKeySystemAccess,
        js_fun_share_nullable_ShareData_Promise_undefined,
        js_fun_canShare_nullable_ShareData_boolean,
        js_fun_taintEnabled__boolean,
        js_fun_registerProtocolHandler_DOMString_DOMString_DOMString_undefined,
        js_fun_registerContentHandler_DOMString_DOMString_DOMString_undefined,
        js_get_mediaSession, js_get_bluetooth, js_get_permissions,
        js_get_mimeTypes, js_get_plugins, js_get_doNotTrack,
        js_get_maxTouchPoints, js_get_mediaCapabilities, js_get_connection,
        js_get_activeVRDisplays, js_get_isWebVRContentDetected,
        js_get_isWebVRContentPresenting, js_get_mediaDevices,
        js_get_serviceWorker, js_get_presentation, js_get_credentials,
        js_get_userActivation, js_get_serial, js_get_wakeLock,
        js_get_scheduling, js_get_clipboard, js_get_appCodeName,
        js_get_appName, js_get_appVersion, js_get_platform,
        js_get_userAgent, js_get_product, js_get_language,
        js_get_languages, js_get_onLine, js_get_hardwareConcurrency,
        js_get_storage, js_get_webdriver, js_get_geolocation
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.BatteryManager.Core
import GHC.Wasm.Web.Generated.Bluetooth.Core
import GHC.Wasm.Web.Generated.BodyInit.Core
import GHC.Wasm.Web.Generated.Clipboard.Core
import GHC.Wasm.Web.Generated.CredentialsContainer.Core
import GHC.Wasm.Web.Generated.Gamepad.Core
import GHC.Wasm.Web.Generated.GamepadServiceTest.Core
import GHC.Wasm.Web.Generated.Geolocation.Core
import GHC.Wasm.Web.Generated.MIDIAccess.Core
import GHC.Wasm.Web.Generated.MIDIOptions.Core
import GHC.Wasm.Web.Generated.MediaCapabilities.Core
import GHC.Wasm.Web.Generated.MediaDevices.Core
import GHC.Wasm.Web.Generated.MediaKeySystemAccess.Core
import GHC.Wasm.Web.Generated.MediaKeySystemConfiguration.Core
import GHC.Wasm.Web.Generated.MediaSession.Core
import GHC.Wasm.Web.Generated.MimeTypeArray.Core
import GHC.Wasm.Web.Generated.Navigator.Core
import GHC.Wasm.Web.Generated.NetworkInformation.Core
import GHC.Wasm.Web.Generated.Permissions.Core
import GHC.Wasm.Web.Generated.PluginArray.Core
import GHC.Wasm.Web.Generated.Presentation.Core
import GHC.Wasm.Web.Generated.Scheduling.Core
import GHC.Wasm.Web.Generated.Serial.Core
import GHC.Wasm.Web.Generated.ServiceWorkerContainer.Core
import GHC.Wasm.Web.Generated.ShareData.Core
import GHC.Wasm.Web.Generated.StorageManager.Core
import GHC.Wasm.Web.Generated.UserActivation.Core
import GHC.Wasm.Web.Generated.VRDisplay.Core
import GHC.Wasm.Web.Generated.VRServiceTest.Core
import GHC.Wasm.Web.Generated.WakeLock.Core
import GHC.Wasm.Web.Types
foreign import javascript safe "$1.getBattery()" js_fun_getBattery__Promise_BatteryManager
  :: Navigator -> (IO (Promise BatteryManagerClass))
foreign import javascript unsafe "$1.vibrate($2)" js_fun_vibrate_long_boolean
  :: Navigator -> (Word32 -> (IO Bool))
foreign import javascript unsafe "$1.vibrate($2)" js_fun_vibrate_sequence_long_boolean
  :: Navigator -> (Sequence (JSPrimClass Word32) -> (IO Bool))
foreign import javascript unsafe "$1.getGamepads()" js_fun_getGamepads__sequence_nullable_Gamepad
  :: Navigator -> (IO (Sequence (NullableClass GamepadClass)))
foreign import javascript unsafe "$1.requestGamepadServiceTest()" js_fun_requestGamepadServiceTest__GamepadServiceTest
  :: Navigator -> (IO GamepadServiceTest)
foreign import javascript safe "$1.getVRDisplays()" js_fun_getVRDisplays__Promise_sequence_VRDisplay
  :: Navigator -> (IO (Promise (SequenceClass VRDisplayClass)))
foreign import javascript unsafe "$1.requestVRPresentation($2)" js_fun_requestVRPresentation_VRDisplay_undefined
  :: Navigator -> (VRDisplay -> (IO ()))
foreign import javascript unsafe "$1.requestVRServiceTest()" js_fun_requestVRServiceTest__VRServiceTest
  :: Navigator -> (IO VRServiceTest)
foreign import javascript safe "$1.requestMIDIAccess($2)" js_fun_requestMIDIAccess_nullable_MIDIOptions_Promise_MIDIAccess
  :: Navigator
     -> (Nullable MIDIOptionsClass -> (IO (Promise MIDIAccessClass)))
foreign import javascript unsafe "$1.sendBeacon($2,$3)" js_fun_sendBeacon_DOMString_nullable_nullable_BodyInit_boolean
  :: Navigator
     -> (DOMString
         -> (Nullable (NullableClass BodyInitClass) -> (IO Bool)))
foreign import javascript safe "$1.requestMediaKeySystemAccess($2,$3)" js_fun_requestMediaKeySystemAccess_DOMString_sequence_MediaKeySystemConfiguration_Promise_MediaKeySystemAccess
  :: Navigator
     -> (DOMString
         -> (Sequence MediaKeySystemConfigurationClass
             -> (IO (Promise MediaKeySystemAccessClass))))
foreign import javascript safe "$1.share($2)" js_fun_share_nullable_ShareData_Promise_undefined
  :: Navigator
     -> (Nullable ShareDataClass -> (IO (Promise UndefinedClass)))
foreign import javascript unsafe "$1.canShare($2)" js_fun_canShare_nullable_ShareData_boolean
  :: Navigator -> (Nullable ShareDataClass -> (IO Bool))
foreign import javascript unsafe "$1.taintEnabled()" js_fun_taintEnabled__boolean
  :: Navigator -> (IO Bool)
foreign import javascript unsafe "$1.registerProtocolHandler($2,$3,$4)" js_fun_registerProtocolHandler_DOMString_DOMString_DOMString_undefined
  :: Navigator
     -> (DOMString -> (DOMString -> (DOMString -> (IO ()))))
foreign import javascript unsafe "$1.registerContentHandler($2,$3,$4)" js_fun_registerContentHandler_DOMString_DOMString_DOMString_undefined
  :: Navigator
     -> (DOMString -> (DOMString -> (DOMString -> (IO ()))))
foreign import javascript unsafe "$1.mediaSession" js_get_mediaSession
  :: Navigator -> (IO MediaSession)
foreign import javascript unsafe "$1.bluetooth" js_get_bluetooth
  :: Navigator -> (IO (Nullable BluetoothClass))
foreign import javascript unsafe "$1.permissions" js_get_permissions
  :: Navigator -> (IO Permissions)
foreign import javascript unsafe "$1.mimeTypes" js_get_mimeTypes
  :: Navigator -> (IO MimeTypeArray)
foreign import javascript unsafe "$1.plugins" js_get_plugins
  :: Navigator -> (IO PluginArray)
foreign import javascript unsafe "$1.doNotTrack" js_get_doNotTrack
  :: Navigator -> (IO DOMString)
foreign import javascript unsafe "$1.maxTouchPoints" js_get_maxTouchPoints
  :: Navigator -> (IO Int32)
foreign import javascript unsafe "$1.mediaCapabilities" js_get_mediaCapabilities
  :: Navigator -> (IO MediaCapabilities)
foreign import javascript unsafe "$1.connection" js_get_connection
  :: Navigator -> (IO NetworkInformation)
foreign import javascript unsafe "$1.activeVRDisplays" js_get_activeVRDisplays
  :: Navigator -> (IO (Sequence VRDisplayClass))
foreign import javascript unsafe "$1.isWebVRContentDetected" js_get_isWebVRContentDetected
  :: Navigator -> (IO Bool)
foreign import javascript unsafe "$1.isWebVRContentPresenting" js_get_isWebVRContentPresenting
  :: Navigator -> (IO Bool)
foreign import javascript unsafe "$1.mediaDevices" js_get_mediaDevices
  :: Navigator -> (IO MediaDevices)
foreign import javascript unsafe "$1.serviceWorker" js_get_serviceWorker
  :: Navigator -> (IO ServiceWorkerContainer)
foreign import javascript unsafe "$1.presentation" js_get_presentation
  :: Navigator -> (IO (Nullable PresentationClass))
foreign import javascript unsafe "$1.credentials" js_get_credentials
  :: Navigator -> (IO CredentialsContainer)
foreign import javascript unsafe "$1.userActivation" js_get_userActivation
  :: Navigator -> (IO UserActivation)
foreign import javascript unsafe "$1.serial" js_get_serial
  :: Navigator -> (IO Serial)
foreign import javascript unsafe "$1.wakeLock" js_get_wakeLock
  :: Navigator -> (IO WakeLock)
foreign import javascript unsafe "$1.scheduling" js_get_scheduling
  :: Navigator -> (IO Scheduling)
foreign import javascript unsafe "$1.clipboard" js_get_clipboard
  :: Navigator -> (IO (Nullable ClipboardClass))
foreign import javascript unsafe "$1.appCodeName" js_get_appCodeName
  :: Navigator -> (IO DOMString)
foreign import javascript unsafe "$1.appName" js_get_appName
  :: Navigator -> (IO DOMString)
foreign import javascript unsafe "$1.appVersion" js_get_appVersion
  :: Navigator -> (IO DOMString)
foreign import javascript unsafe "$1.platform" js_get_platform
  :: Navigator -> (IO DOMString)
foreign import javascript unsafe "$1.userAgent" js_get_userAgent
  :: Navigator -> (IO DOMString)
foreign import javascript unsafe "$1.product" js_get_product
  :: Navigator -> (IO DOMString)
foreign import javascript unsafe "$1.language" js_get_language
  :: Navigator -> (IO (Nullable DOMStringClass))
foreign import javascript unsafe "$1.languages" js_get_languages
  :: Navigator -> (IO (Sequence DOMStringClass))
foreign import javascript unsafe "$1.onLine" js_get_onLine
  :: Navigator -> (IO Bool)
foreign import javascript unsafe "$1.hardwareConcurrency" js_get_hardwareConcurrency
  :: Navigator -> (IO Word64)
foreign import javascript unsafe "$1.storage" js_get_storage
  :: Navigator -> (IO StorageManager)
foreign import javascript unsafe "$1.webdriver" js_get_webdriver
  :: Navigator -> (IO Bool)
foreign import javascript unsafe "$1.geolocation" js_get_geolocation
  :: Navigator -> (IO Geolocation)
