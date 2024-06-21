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
module GHC.Wasm.Web.Generated.SubtleCrypto (
        SubtleCrypto, SubtleCryptoClass,
        js_fun_encrypt_AlgorithmIdentifier_CryptoKey_BufferSource_Promise_any,
        js_fun_decrypt_AlgorithmIdentifier_CryptoKey_BufferSource_Promise_any,
        js_fun_sign_AlgorithmIdentifier_CryptoKey_BufferSource_Promise_any,
        js_fun_verify_AlgorithmIdentifier_CryptoKey_BufferSource_BufferSource_Promise_any,
        js_fun_digest_AlgorithmIdentifier_BufferSource_Promise_any,
        js_fun_generateKey_AlgorithmIdentifier_boolean_sequence_KeyUsage_Promise_any,
        js_fun_deriveKey_AlgorithmIdentifier_CryptoKey_AlgorithmIdentifier_boolean_sequence_KeyUsage_Promise_any,
        js_fun_deriveBits_AlgorithmIdentifier_CryptoKey_long_Promise_any,
        js_fun_importKey_KeyFormat_object_AlgorithmIdentifier_boolean_sequence_KeyUsage_Promise_any,
        js_fun_exportKey_KeyFormat_CryptoKey_Promise_any,
        js_fun_wrapKey_KeyFormat_CryptoKey_CryptoKey_AlgorithmIdentifier_Promise_any,
        js_fun_unwrapKey_KeyFormat_BufferSource_CryptoKey_AlgorithmIdentifier_AlgorithmIdentifier_boolean_sequence_KeyUsage_Promise_any
    ) where
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.AlgorithmIdentifier.Core
import GHC.Wasm.Web.Generated.CryptoKey.Core
import GHC.Wasm.Web.Generated.KeyFormat.Core
import GHC.Wasm.Web.Generated.KeyUsage.Core
import GHC.Wasm.Web.Generated.SubtleCrypto.Core
import GHC.Wasm.Web.Types
foreign import javascript safe "$1.encrypt($2,$3,$4)" js_fun_encrypt_AlgorithmIdentifier_CryptoKey_BufferSource_Promise_any
  :: SubtleCrypto
     -> (AlgorithmIdentifier
         -> (CryptoKey -> (BufferSource -> (IO (Promise AnyClass)))))
foreign import javascript safe "$1.decrypt($2,$3,$4)" js_fun_decrypt_AlgorithmIdentifier_CryptoKey_BufferSource_Promise_any
  :: SubtleCrypto
     -> (AlgorithmIdentifier
         -> (CryptoKey -> (BufferSource -> (IO (Promise AnyClass)))))
foreign import javascript safe "$1.sign($2,$3,$4)" js_fun_sign_AlgorithmIdentifier_CryptoKey_BufferSource_Promise_any
  :: SubtleCrypto
     -> (AlgorithmIdentifier
         -> (CryptoKey -> (BufferSource -> (IO (Promise AnyClass)))))
foreign import javascript safe "$1.verify($2,$3,$4,$5)" js_fun_verify_AlgorithmIdentifier_CryptoKey_BufferSource_BufferSource_Promise_any
  :: SubtleCrypto
     -> (AlgorithmIdentifier
         -> (CryptoKey
             -> (BufferSource -> (BufferSource -> (IO (Promise AnyClass))))))
foreign import javascript safe "$1.digest($2,$3)" js_fun_digest_AlgorithmIdentifier_BufferSource_Promise_any
  :: SubtleCrypto
     -> (AlgorithmIdentifier
         -> (BufferSource -> (IO (Promise AnyClass))))
foreign import javascript safe "$1.generateKey($2,$3,$4)" js_fun_generateKey_AlgorithmIdentifier_boolean_sequence_KeyUsage_Promise_any
  :: SubtleCrypto
     -> (AlgorithmIdentifier
         -> (Bool -> (Sequence KeyUsageClass -> (IO (Promise AnyClass)))))
foreign import javascript safe "$1.deriveKey($2,$3,$4,$5,$6)" js_fun_deriveKey_AlgorithmIdentifier_CryptoKey_AlgorithmIdentifier_boolean_sequence_KeyUsage_Promise_any
  :: SubtleCrypto
     -> (AlgorithmIdentifier
         -> (CryptoKey
             -> (AlgorithmIdentifier
                 -> (Bool -> (Sequence KeyUsageClass -> (IO (Promise AnyClass)))))))
foreign import javascript safe "$1.deriveBits($2,$3,$4)" js_fun_deriveBits_AlgorithmIdentifier_CryptoKey_long_Promise_any
  :: SubtleCrypto
     -> (AlgorithmIdentifier
         -> (CryptoKey -> (Word32 -> (IO (Promise AnyClass)))))
foreign import javascript safe "$1.importKey($2,$3,$4,$5,$6)" js_fun_importKey_KeyFormat_object_AlgorithmIdentifier_boolean_sequence_KeyUsage_Promise_any
  :: SubtleCrypto
     -> (KeyFormat
         -> (JSAny
             -> (AlgorithmIdentifier
                 -> (Bool -> (Sequence KeyUsageClass -> (IO (Promise AnyClass)))))))
foreign import javascript safe "$1.exportKey($2,$3)" js_fun_exportKey_KeyFormat_CryptoKey_Promise_any
  :: SubtleCrypto
     -> (KeyFormat -> (CryptoKey -> (IO (Promise AnyClass))))
foreign import javascript safe "$1.wrapKey($2,$3,$4,$5)" js_fun_wrapKey_KeyFormat_CryptoKey_CryptoKey_AlgorithmIdentifier_Promise_any
  :: SubtleCrypto
     -> (KeyFormat
         -> (CryptoKey
             -> (CryptoKey
                 -> (AlgorithmIdentifier -> (IO (Promise AnyClass))))))
foreign import javascript safe "$1.unwrapKey($2,$3,$4,$5,$6,$7,$8)" js_fun_unwrapKey_KeyFormat_BufferSource_CryptoKey_AlgorithmIdentifier_AlgorithmIdentifier_boolean_sequence_KeyUsage_Promise_any
  :: SubtleCrypto
     -> (KeyFormat
         -> (BufferSource
             -> (CryptoKey
                 -> (AlgorithmIdentifier
                     -> (AlgorithmIdentifier
                         -> (Bool
                             -> (Sequence KeyUsageClass -> (IO (Promise AnyClass)))))))))
