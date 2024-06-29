{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- | Bindings to the Crypto API available in Cloudflare Workers
and some utilities to verify Cloudflare Zero Trust JWT.
-}
module Network.Cloudflare.Worker.Crypto (
  JSObject (..),
  subtleCrypto,
  randomUUID,
  getCloudflarePublicKeys,
  verifyRS256,
  crypto,
  CloudflarePubKey (..),
) where

import Control.Arrow ((&&&))
import Control.Exception.Safe (throwString)
import Data.Aeson (FromJSON (..))
import Data.Aeson qualified as J
import Data.Aeson.Parser qualified as AA
import Data.Attoparsec.ByteString.Streaming qualified as AQ
import Data.Bitraversable (bitraverse)
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Lazy qualified as LBS
import Data.Functor.Const (Const (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import Data.Word (Word8)
import GHC.Generics (Generic)
import GHC.IO (unsafePerformIO)
import GHC.Natural (Natural)
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Prim
import GHC.Wasm.Web.Generated.AlgorithmIdentifier (AlgorithmIdentifier)
import GHC.Wasm.Web.Generated.Crypto
import GHC.Wasm.Web.Generated.CryptoKey (CryptoKey)
import GHC.Wasm.Web.Generated.Response qualified as Resp
import GHC.Wasm.Web.Generated.SubtleCrypto (SubtleCrypto, js_fun_importKey_KeyFormat_object_AlgorithmIdentifier_boolean_sequence_KeyUsage_Promise_any, js_fun_verify_AlgorithmIdentifier_CryptoKey_BufferSource_BufferSource_Promise_any)
import GHC.Wasm.Web.ReadableStream (fromReadableStream)
import Network.Cloudflare.Worker.FetchAPI qualified as Fetch
import Streaming.ByteString qualified as Q

foreign import javascript unsafe "crypto.subtle"
  subtleCrypto :: SubtleCrypto

foreign import javascript unsafe "crypto"
  crypto :: Crypto

randomUUID :: IO String
randomUUID = fromJSString . convertToJSString <$> js_fun_randomUUID__DOMString crypto

foreign import javascript unsafe "{name: \"RSASSA-PKCS1-v1_5\", hash: \"SHA-256\"}"
  rs256 :: AlgorithmIdentifier

fromCloudflarePubKey :: CloudflarePubKey -> IO CryptoKey
fromCloudflarePubKey pk = do
  jwk <- fromHaskellByteString $ LBS.toStrict $ J.encode pk
  fmap unsafeCast . await
    =<< js_fun_importKey_KeyFormat_object_AlgorithmIdentifier_boolean_sequence_KeyUsage_Promise_any
      subtleCrypto
      (toDOMString False $ toJSString "jwk")
      (upcast jwk)
      rs256
      True
      (toSequence $ V.singleton $ toDOMString False $ toJSString "verify")

type Signature = BS.ByteString

type Message = BS.ByteString

verifyRS256 :: CloudflarePubKey -> Signature -> Message -> Bool
verifyRS256 pk sig msg = unsafePerformIO do
  key <- fromCloudflarePubKey pk
  useByteStringAsJSByteArray @Word8 sig \sig' ->
    useByteStringAsJSByteArray @Word8 msg \msg' ->
      fmap (fromJSPrim . unsafeCast @_ @(JSPrimClass Bool)) . await
        =<< js_fun_verify_AlgorithmIdentifier_CryptoKey_BufferSource_BufferSource_Promise_any
          subtleCrypto
          (inject $ toDOMString False $ toJSString "RSASSA-PKCS1-v1_5")
          key
          (inject sig')
          (inject msg')

data CloudflarePubKey = CloudflarePubKey
  { keyId :: T.Text
  , pubkeyN :: Natural
  , pubkeyE :: Natural
  }
  deriving (Show, Eq, Ord, Generic)

data CloudflareCerts = CloudflareCerts {keys :: [CloudflarePubKey]}
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON)

newtype BigEndian = BigEndian {rawNatural :: Natural}
  deriving (Show, Eq, Ord, Generic)

instance FromJSON BigEndian where
  parseJSON = J.withText "BigEndian" \txt -> do
    !raw <- either (fail . ("BigEndian: " <>)) pure $ B64.decode $ TE.encodeUtf8 txt
    let !n = BS.foldl' (\acc x -> acc * 256 + fromIntegral x) 0 raw
    pure $ BigEndian n

-- | In JWK Format
instance FromJSON CloudflarePubKey where
  parseJSON = J.withObject "JWK" \dic -> do
    keyId <- dic J..: "kid"
    "RSA" :: T.Text <- dic J..: "kty"
    "RS256" :: T.Text <- dic J..: "alg"
    "sig" :: T.Text <- dic J..: "use"
    BigEndian pubkeyN <- dic J..: "n"
    BigEndian pubkeyE <- dic J..: "e"
    pure CloudflarePubKey {keyId, pubkeyN, pubkeyE}

instance J.ToJSON BigEndian where
  toJSON = J.toJSON . TE.decodeASCII . B64.encode . naturalBE . rawNatural

naturalBE :: Natural -> BS.ByteString
naturalBE = LBS.toStrict . BB.toLazyByteString . getConst . go
  where
    go 0 = pure ()
    go n =
      let (q, r) = n `quotRem` 256
       in go q *> Const (BB.word8 $ fromIntegral r)

instance J.ToJSON CloudflarePubKey where
  toJSON CloudflarePubKey {..} =
    J.object
      [ "kid" J..= keyId
      , "kty" J..= ("RSA" :: T.Text)
      , "alg" J..= ("RS256" :: T.Text)
      , "use" J..= ("sig" :: T.Text)
      , "n" J..= BigEndian pubkeyN
      , "e" J..= BigEndian pubkeyE
      ]

type TeamName = String

getCloudflarePublicKeys :: TeamName -> IO (Map T.Text CloudflarePubKey)
getCloudflarePublicKeys team = do
  rsp <-
    await
      =<< Fetch.get ("https://" <> team <> ".cloudflareaccess.com/cdn-cgi/access/certs")
  (val, ()) <-
    bitraverse (either (throwString . show) pure) Q.effects
      =<< maybe
        (throwString "Empty Body returned for Cloudflare certs!")
        (AQ.parse AA.json' . fromReadableStream)
        . fromNullable
      =<< Resp.js_get_body rsp
  case J.fromJSON val of
    J.Error e -> throwString $ "Error during parsing cf keys: " <> e
    J.Success CloudflareCerts {..} ->
      pure $ Map.fromList $ map (keyId &&& id) keys
