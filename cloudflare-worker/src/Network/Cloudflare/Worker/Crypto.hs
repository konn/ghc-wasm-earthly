{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

{- | Bindings to the Crypto API available in Cloudflare Workers
and some utilities to verify Cloudflare Zero Trust JWT.
-}
module Network.Cloudflare.Worker.Crypto (
  -- * Binding to the standard Crypto-related APIs
  crypto,
  subtleCrypto,
  randomUUID,

  -- * Combinators for verifying JWT provided by Cloudflare Zero Trust
  getCloudflarePublicKeys,
  TeamName,
  CloudflareAudienceID,
  CloudflarePubKey (..),
  CloudflarePubKeys,
  verifyCloudflareJWTAssertion,
  CloudflareUser (..),

  -- ** Low-level combinators
  parseJWT,
  JWTToken (..),
  verifyJWT,
  verifyRS256,

  -- * Re-exports
  JSObject (..),
) where

import Control.Applicative ((<|>))
import Control.Arrow ((&&&), (>>>))
import Control.Exception.Safe (throwString)
import Control.Monad (forM_, unless, when)
import Data.Aeson (FromJSON (..))
import Data.Aeson qualified as J
import Data.Aeson.Parser qualified as AA
import Data.Attoparsec.ByteString.Streaming qualified as AQ
import Data.Bifunctor qualified as Bi
import Data.Bitraversable (bitraverse)
import Data.ByteString qualified as BS
import Data.ByteString.Base64.URL qualified as B64
import Data.ByteString.Char8 qualified as BS8
import Data.CaseInsensitive qualified as CI
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Vector qualified as V
import Data.Word (Word8)
import GHC.Exts (IsList)
import GHC.Generics (Generic)
import GHC.IO (unsafePerformIO)
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.AlgorithmIdentifier (AlgorithmIdentifier)
import GHC.Wasm.Web.Generated.Crypto
import GHC.Wasm.Web.Generated.CryptoKey (CryptoKey)
import GHC.Wasm.Web.Generated.JsonWebKey (JsonWebKey)
import GHC.Wasm.Web.Generated.Response qualified as Resp
import GHC.Wasm.Web.Generated.SubtleCrypto (SubtleCrypto, js_fun_importKey_KeyFormat_object_AlgorithmIdentifier_boolean_sequence_KeyUsage_Promise_any, js_fun_verify_AlgorithmIdentifier_CryptoKey_BufferSource_BufferSource_Promise_any)
import GHC.Wasm.Web.ReadableStream (fromReadableStream)
import Network.Cloudflare.Worker.FetchAPI qualified as Fetch
import Network.Cloudflare.Worker.Request (WorkerRequest)
import Network.Cloudflare.Worker.Request qualified as Req
import Streaming.ByteString qualified as Q
import Wasm.Data.Function.Linear qualified as PL

randomUUID :: IO T.Text
randomUUID = toText <$> js_fun_randomUUID__DOMString crypto

fromCloudflarePubKey :: CloudflarePubKey -> IO CryptoKey
fromCloudflarePubKey pk = do
  let jwk = toJWK pk
  fmap unsafeCast . await
    =<< js_fun_importKey_KeyFormat_object_AlgorithmIdentifier_boolean_sequence_KeyUsage_Promise_any
      subtleCrypto
      "jwk"
      (upcast jwk)
      rs256
      False
      (toSequence $ V.singleton "verify")

toJWK :: CloudflarePubKey -> JsonWebKey
toJWK pk =
  newDictionary
    ( setPartialField "n" (nonNull $ fromText $ pk.pubkeyN.rawBE)
        PL.. setPartialField "e" (nonNull $ fromText pk.pubkeyE.rawBE)
        PL.. setPartialField "kty" "RSA"
        PL.. setPartialField "use" (nonNull "sig")
        PL.. setPartialField "alg" (nonNull "RS256")
    )

data Alg = RS256
  deriving (Show, Eq, Ord, Generic)

instance J.FromJSON Alg where
  parseJSON =
    J.withText "Alg" $
      CI.mk >>> \case
        "RS256" -> pure RS256
        _ -> fail "Invalid Alg"

instance J.ToJSON Alg where
  toJSON = J.toJSON . show

data TokenType = JWT
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, J.ToJSON)

data RawJWTToken = RawJWTToken
  { header :: RawTokenHeader
  , decodedHeader :: BS.ByteString
  , payload :: RawTokenPayload
  , decodedPayload :: BS.ByteString
  , signature :: Signature
  }
  deriving (Show, Eq, Ord, Generic)

data JWTToken = JWTToken
  { header :: AppTokenHeader
  , payload :: AppTokenPayload
  }
  deriving (Show, Eq, Ord, Generic)

type RawTokenHeader = BS.ByteString

type RawTokenPayload = BS.ByteString

verifyJWT ::
  POSIXTime ->
  Map T.Text CloudflarePubKey ->
  RawJWTToken ->
  Either String JWTToken
verifyJWT now keys toks = do
  header <-
    Bi.first ("Invalid Header (JSON): " <>) $
      J.eitherDecodeStrict' toks.decodedHeader
  payload <-
    Bi.first ("Invalid Payload (JSON): " <>) $
      J.eitherDecodeStrict' toks.decodedPayload
  key <-
    maybe (Left $ "Key not found: " <> T.unpack header.kid) pure $
      Map.lookup header.kid keys
  let msg = toks.header <> "." <> toks.payload
  unless (verifyRS256 key toks.signature msg) $
    Left "Invalid signature"
  verifyTimestamps now payload
  pure JWTToken {..}

verifyTimestamps :: POSIXTime -> AppTokenPayload -> Either String ()
verifyTimestamps now pay = do
  when (now < pay.iat) do
    Left "Token issued in the future"
  when (now > pay.exp) do
    Left "Token expired"
  forM_ pay.nbf \nbf -> when (now < nbf) do
    Left "Token not yet valid"

parseJWT :: BS.ByteString -> Either String RawJWTToken
parseJWT raw = Bi.first (("Error during parsing token (" <> BS8.unpack raw <> "):") <>) $
  case BS8.split '.' raw of
    [header, payload, sigB64] -> do
      decodedHeader <-
        Bi.first ("Invalid Header (Base65): " <>) $ decodeB64Pad header
      Bi.first ("Invalid Header: " <>) $ validateRawJSON decodedHeader
      decodedPayload <-
        Bi.first ("Invalid payload (Base64): " <>) (decodeB64Pad payload)
      Bi.first ("Invalid Payload: " <>) $ validateRawJSON decodedPayload
      signature <- Bi.first ("Invalid signature (Base64): " <>) $ decodeB64Pad sigB64

      pure RawJWTToken {..}
    _ -> Left "Invalid JWT String"

decodeB64Pad :: BS8.ByteString -> Either String Signature
decodeB64Pad = B64.decode . pad
  where
    pad bs =
      let n = BS.length bs
          pads = BS8.replicate ((-n) `rem` 4) '='
       in bs <> pads

validateRawJSON :: BS.ByteString -> Either String ()
validateRawJSON raw =
  if BS8.any (`elem` ("\r\n\t " :: [Char])) raw
    then Left "JSON contains whitespaces"
    else Right ()

data AppTokenHeader = AppTokenHeader {alg :: !Alg, kid :: !T.Text}
  deriving (Show, Eq, Ord, Generic)
  deriving (J.FromJSON, J.ToJSON)

data AppTokenPayload = AppTokenPayload
  { aud :: !Audiences
  , email :: !(Maybe T.Text)
  , exp :: !POSIXTime
  , iat :: !POSIXTime
  , nbf :: !(Maybe POSIXTime)
  , iss :: !T.Text
  , type_ :: !T.Text
  , identity_nonce :: !(Maybe T.Text)
  , sub :: !T.Text
  , country :: !(Maybe T.Text)
  }
  deriving (Show, Eq, Ord, Generic)

newtype Audiences = Audiences {aud :: [T.Text]}
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (IsList)

instance FromJSON Audiences where
  parseJSON obj =
    Audiences <$> J.parseJSON obj
      <|> Audiences . pure <$> J.parseJSON obj

instance J.ToJSON Audiences where
  toJSON (Audiences xs) = J.toJSON xs

appTokenOpts :: J.Options
appTokenOpts = J.defaultOptions {J.fieldLabelModifier = T.unpack . T.dropWhileEnd (== '_') . T.pack}

instance J.FromJSON AppTokenPayload where
  parseJSON = J.genericParseJSON appTokenOpts

instance J.ToJSON AppTokenPayload where
  toJSON = J.genericToJSON appTokenOpts

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
          (inject ("RSASSA-PKCS1-v1_5" :: DOMString))
          key
          (inject sig')
          (inject msg')

data CloudflarePubKey = CloudflarePubKey
  { keyId :: T.Text
  , pubkeyN :: BigEndian
  , pubkeyE :: BigEndian
  }
  deriving (Show, Eq, Ord, Generic)

data CloudflareCerts = CloudflareCerts {keys :: [CloudflarePubKey]}
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON)

newtype BigEndian = BigEndian {rawBE :: T.Text}
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (FromJSON, J.ToJSON)

-- | In JWK Format
instance FromJSON CloudflarePubKey where
  parseJSON = J.withObject "JWK" \dic -> do
    keyId <- dic J..: "kid"
    "RSA" :: T.Text <- dic J..: "kty"
    RS256 <- dic J..: "alg"
    "sig" :: T.Text <- dic J..: "use"
    pubkeyN <- dic J..: "n"
    pubkeyE <- dic J..: "e"
    pure CloudflarePubKey {keyId, pubkeyN, pubkeyE}

instance J.ToJSON CloudflarePubKey where
  toJSON CloudflarePubKey {..} =
    J.object
      [ "kid" J..= keyId
      , "kty" J..= ("RSA" :: T.Text)
      , "alg" J..= ("RS256" :: T.Text)
      , "use" J..= ("sig" :: T.Text)
      , "n" J..= pubkeyN
      , "e" J..= pubkeyE
      ]

type TeamName = String

data CloudflareUser = CloudflareUser {email :: Maybe T.Text, country :: Maybe T.Text, type_ :: T.Text}
  deriving (Show, Eq, Ord, Generic)

instance J.FromJSON CloudflareUser where
  parseJSON = J.genericParseJSON appTokenOpts

instance J.ToJSON CloudflareUser where
  toJSON = J.genericToJSON appTokenOpts

type CloudflareAudienceID = T.Text

verifyCloudflareJWTAssertion ::
  POSIXTime ->
  CloudflareAudienceID ->
  CloudflarePubKeys ->
  WorkerRequest ->
  Either String CloudflareUser
verifyCloudflareJWTAssertion now aud keys req = do
  let hdrName = "Cf-Access-Jwt-Assertion"
  val <-
    maybe (Left $ "No " <> BS8.unpack hdrName <> " header given") Right $
      lookup (CI.mk hdrName) $
        map (Bi.first CI.mk) $
          Req.getHeaders req
  parsed <- parseJWT val
  tok <- verifyJWT now keys parsed
  if aud `elem` tok.payload.aud.aud
    then
      pure
        CloudflareUser
          { email = tok.payload.email
          , country = tok.payload.country
          , type_ = tok.payload.type_
          }
    else Left "Invalid Audience"

type CloudflarePubKeys = Map T.Text CloudflarePubKey

getCloudflarePublicKeys :: TeamName -> IO CloudflarePubKeys
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

foreign import javascript unsafe "crypto.subtle"
  subtleCrypto :: SubtleCrypto

foreign import javascript unsafe "crypto"
  crypto :: Crypto

foreign import javascript unsafe "{name: \"RSASSA-PKCS1-v1_5\", hash: \"SHA-256\"}"
  rs256 :: AlgorithmIdentifier
