{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnliftedDatatypes #-}
{-# LANGUAGE NoFieldSelectors #-}

module Network.Cloudflare.Worker.Binding.D1 (
  -- * Core Types
  D1,
  D1Class,
  Statement,
  StatementClass,
  D1ValueClass,
  D1Error (..),

  -- * Values and Rows
  D1Value,
  D1ValueAlts,
  D1ValueView (..),
  FromD1Value (..),
  ToD1Value (..),
  unviewD1Value,
  viewD1Value,
  D1Row,
  D1RowClass,
  D1RowView (..),
  ToD1Row (..),
  GenericToD1Row,
  genericToD1Row,
  genericToD1RowView,
  FromD1Row (..),
  GenericFromD1Row,
  genericFromD1Row,
  genericFromD1RowView,
  viewD1Row,
  unviewD1Row,

  -- * Prepared statements
  PreparedStatement,
  PreparedStatementClass,
  prepare,
  bind,
  bind',

  -- * Queries

  -- ** Types for query resulsts
  D1Result,
  D1ResultClass,
  D1ResultFields,
  D1ResultView (..),
  D1MetadataClass,
  D1Metadata,
  D1MetadataView (..),

  -- ** Lists all rows
  all,
  all',

  -- ** Lists all rows, but witout squashing columns
  raw,
  raw',
  rawWithColumns,
  rawWithColumns',

  -- ** Fetches the first row or columns
  first,
  first',
  firstColumns,
  firstColumns',

  -- ** Runs a query without results
  run,
  run',
  D1Metrics,
  D1MetricsFields,
  D1MetricsClass,
  D1MetricsView (..),

  -- ** Batch run
  batch,

  -- ** Executes a raw queries.
  exec,
  exec',
  D1ExecResult,
  D1ExecResultView (..),
  D1ExecResultClass,
  D1ExecResultFields,
) where

import Control.Exception
import Control.Monad ((<=<))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.CaseInsensitive qualified as CI
import Data.Coerce (coerce)
import Data.Int (Int16, Int32, Int8)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Data.Time (UTCTime)
import Data.Time.Format.ISO8601 (iso8601ParseM, iso8601Show)
import Data.Vector qualified as V
import Data.Word
import Effectful.Concurrent.Async (Async)
import GHC.Exts (noinline, proxy#)
import GHC.Generics (Generic (..), Generically (..), K1 (..), M1 (..), Meta (..), S, (:*:) (..), (:+:))
import GHC.IO.Unsafe (unsafeDupablePerformIO, unsafePerformIO)
import GHC.TypeError (Unsatisfiable, unsatisfiable)
import GHC.TypeLits
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Object.Builtins.Buffer qualified as Buffer
import GHC.Wasm.Prim
import Wasm.Prelude.Linear qualified as PL
import Prelude hiding (all)

-- TODO: JSON-related interops https://developers.cloudflare.com/d1/build-with-d1/query-json/

----------------
-- Core types
----------------

type data D1Class :: Prototype

type instance SuperclassOf D1Class = 'Nothing

type D1 = JSObject D1Class

type D1MetricsClass = JSDictionaryClass D1MetricsFields

type D1MetricsFields = '[ '("success", JSPrimClass Bool), '("meta", D1MetadataClass)]

data D1MetricsView = D1MetricsView
  { success :: Bool
  , meta :: D1MetadataView
  }
  deriving (Show, Eq, Ord, Generic)

viewMetrics :: D1Metrics -> D1MetricsView
viewMetrics m = unsafeDupablePerformIO do
  m' <- reifyDictionary m
  pure D1MetricsView {success = fromJSPrim m'.success, meta = viewMetadata m'.meta}

type D1Metrics = JSObject D1MetricsClass

type data PreparedStatementClass :: Prototype

type instance SuperclassOf PreparedStatementClass = 'Nothing

type PreparedStatement = JSObject PreparedStatementClass

type data StatementClass :: Prototype

type instance SuperclassOf StatementClass = 'Nothing

type Statement = JSObject StatementClass

prepare :: D1 -> String -> IO PreparedStatement
prepare d1 stmt = js_d1_prepare d1 (toJSString stmt)

type D1ValueAlts =
  '[ JSPrimClass Int32
   , JSPrimClass Double
   , USVStringClass
   , JSByteArrayClass Word8
   ]

type D1ValueClass = NullableClass (UnionClass D1ValueAlts)

type D1Value = JSObject D1ValueClass

data D1ValueView
  = D1Null
  | D1Real !Double
  | D1Int !Int32
  | D1Text !T.Text
  | D1Blob !BS.ByteString
  deriving (Show, Eq, Ord, Generic)

bind :: PreparedStatement -> V.Vector D1ValueView -> IO Statement
bind stmt = js_d1_bind stmt . toSequence . V.map unviewD1Value

bind' :: PreparedStatement -> V.Vector D1Value -> IO Statement
bind' stmt = js_d1_bind stmt . toSequence

unviewD1Value :: D1ValueView -> D1Value
unviewD1Value D1Null = upcast jsNull
unviewD1Value (D1Real x) = upcast $ inject @_ @D1ValueAlts $ toJSPrim x
unviewD1Value (D1Int x) = upcast $ inject @_ @D1ValueAlts $ toJSPrim x
unviewD1Value (D1Text txt) = upcast $ inject @USVStringClass @D1ValueAlts $ fromText txt
unviewD1Value (D1Blob bs) =
  upcast $
    inject @(JSByteArrayClass Word8) @D1ValueAlts $
      noinline $
        unsafePerformIO $
          useByteStringAsJSByteArray bs pure

viewD1Value :: D1Value -> D1ValueView
viewD1Value v
  | js_is_null v = D1Null
  | js_is_number v =
      if js_is_integer v
        then D1Int $ fromJSPrim $ unsafeCast v
        else D1Real $ fromJSPrim $ unsafeCast v
  | js_is_string v =
      D1Text $ toText @USVStringClass $ unsafeCast v
  | otherwise =
      -- Must be ArrayBuffer, corresponding to BLOB
      D1Blob $ Buffer.toByteString @Word8 $ noinline (unsafePerformIO $ fromArrayBuffer $ unsafeCast v)

unviewD1Row :: D1RowView -> D1Row
unviewD1Row (D1RowView dic) = unsafeDupablePerformIO do
  toJSRecord $ unviewD1Value <$> dic

viewD1Row :: D1Row -> D1RowView
viewD1Row row = unsafeDupablePerformIO do
  dic <- fromJSRecord row
  pure $ D1RowView $ viewD1Value <$> dic

type GenericFromD1Row a = (Generic a, GFromD1Row (Rep a))

instance (GenericFromD1Row a) => FromD1Row (Generically a) where
  parseD1Row = fmap Generically . genericFromD1Row
  {-# INLINE parseD1Row #-}
  parseD1RowView = fmap Generically . genericFromD1RowView
  {-# INLINE parseD1RowView #-}

genericFromD1Row :: (GenericFromD1Row a) => D1Row -> Either String a
genericFromD1Row = fmap to . gparseD1Row

genericFromD1RowView :: (GenericFromD1Row a) => D1RowView -> Either String a
genericFromD1RowView = fmap to . gparseD1RowView . (.getD1RowView)

class FromD1Row a where
  parseD1Row :: D1Row -> Either String a
  default parseD1Row :: (GenericFromD1Row a) => D1Row -> Either String a
  parseD1Row = genericFromD1Row
  parseD1RowView :: D1RowView -> Either String a
  default parseD1RowView :: (GenericFromD1Row a) => D1RowView -> Either String a
  parseD1RowView = genericFromD1RowView

class GFromD1Row a where
  gparseD1Row :: D1Row -> Either String (a ())
  gparseD1RowView :: Map T.Text D1ValueView -> Either String (a ())

instance
  {-# OVERLAPPING #-}
  (ms ~ 'Just s, KnownSymbol s, FromD1Value c) =>
  GFromD1Row (M1 S ('MetaSel ms a b x) (K1 i c))
  where
  gparseD1Row =
    fmap (M1 . K1) . parseD1Value . js_row_get (toJSString $ symbolVal' @s proxy#)
  gparseD1RowView =
    let sel = symbolVal' @s proxy#
     in maybe
          (Left $ "Column not found: " <> sel)
          (fmap (M1 . K1) . parseD1ValueView)
          . Map.lookup (T.pack sel)

instance {-# OVERLAPPABLE #-} (GFromD1Row f) => GFromD1Row (M1 i c f) where
  gparseD1Row = fmap M1 . gparseD1Row
  gparseD1RowView = fmap M1 . gparseD1RowView

instance (GFromD1Row f, GFromD1Row g) => GFromD1Row (f :*: g) where
  gparseD1Row row = do
    (:*:) <$> gparseD1Row row <*> gparseD1Row row
  gparseD1RowView dic = do
    (:*:) <$> gparseD1RowView dic <*> gparseD1RowView dic

instance (Unsatisfiable ('Text "Sum type cannot be parsed from a row!")) => GFromD1Row (f :+: g) where
  gparseD1Row = unsatisfiable
  gparseD1RowView = unsatisfiable

instance FromD1Row D1Row where
  parseD1Row = Right
  parseD1RowView = Right . unviewD1Row

instance FromD1Row D1RowView where
  parseD1Row = Right . viewD1Row
  parseD1RowView = Right

type GenericToD1Row a = (Generic a, GToD1Row (Rep a))

allocD1Row :: (PartialRow %1 -> PartialRow) %1 -> D1Row
allocD1Row f =
  f js_new_row PL.& \case
    PartialRow a -> a

genericToD1RowView :: (GenericToD1Row a) => a -> D1RowView
{-# INLINE genericToD1RowView #-}
genericToD1RowView x = D1RowView $ gtoD1RowView (from x) mempty

genericToD1Row :: (GenericToD1Row a) => a -> D1Row
{-# INLINE genericToD1Row #-}
genericToD1Row x = allocD1Row (gtoD1Row $ from x)

instance (GenericToD1Row a) => ToD1Row (Generically a) where
  toD1Row = genericToD1Row . coerce @(Generically a) @a
  {-# INLINE toD1Row #-}
  toD1RowView = genericToD1RowView . coerce @(Generically a) @a
  {-# INLINE toD1RowView #-}

newtype PartialRow = PartialRow D1Row

class GToD1Row f where
  gtoD1Row :: f () -> PartialRow %1 -> PartialRow
  gtoD1RowView :: f () -> Map T.Text D1ValueView -> Map T.Text D1ValueView

instance
  {-# OVERLAPPING #-}
  (ms ~ 'Just s, KnownSymbol s, ToD1Value c) =>
  GToD1Row (M1 S ('MetaSel ms a b x) (K1 i c))
  where
  gtoD1Row (M1 (K1 x)) =
    js_partial_set (toJSString $ symbolVal' @s proxy#) (toD1Value x)
  gtoD1RowView (M1 (K1 x)) =
    Map.insert (T.pack $ symbolVal' @s proxy#) (toD1ValueView x)

instance {-# OVERLAPPABLE #-} (GToD1Row f) => GToD1Row (M1 i c f) where
  gtoD1Row = gtoD1Row . unM1
  gtoD1RowView = gtoD1RowView . unM1

instance
  (Unsatisfiable ('Text "Sum type cannot be encoded to a row!")) =>
  GToD1Row (f :+: g)
  where
  gtoD1Row = unsatisfiable
  gtoD1RowView = unsatisfiable

instance (GToD1Row f, GToD1Row g) => GToD1Row (f :*: g) where
  gtoD1Row (f :*: g) = gtoD1Row f PL.. gtoD1Row g
  gtoD1RowView (f :*: g) = gtoD1RowView f . gtoD1RowView g

class ToD1Row a where
  toD1Row :: a -> D1Row
  default toD1Row :: (GenericToD1Row a) => a -> D1Row
  toD1Row = genericToD1Row
  toD1RowView :: a -> D1RowView
  default toD1RowView :: (GenericToD1Row a) => a -> D1RowView
  toD1RowView = genericToD1RowView

instance ToD1Row D1Row where
  toD1Row = id
  toD1RowView = viewD1Row

instance ToD1Row D1RowView where
  toD1Row = unviewD1Row
  toD1RowView = id

all :: Statement -> IO (Async (D1ResultView D1RowView))
all = deferWith (fromResults viewD1Row) <=< js_all

fromResults :: (JSObject cls -> a) -> D1Result cls -> D1ResultView a
fromResults f r = unsafeDupablePerformIO do
  !row <- reifyDictionary r
  results <- V.map f <$> toVector row.results
  pure
    D1ResultView
      { results
      , success = fromJSPrim row.success
      , meta = viewMetadata row.meta
      }

viewMetadata :: D1Metadata -> D1MetadataView
viewMetadata meta = unsafeDupablePerformIO do
  meta' <- reifyDictionary meta
  let duration = fromJSPrim meta'.duration
      rowsRead = nullable Nothing (Just . fromJSPrim) meta'.rows_read
      rowsWritten = nullable Nothing (Just . fromJSPrim) meta'.rows_written
  pure D1MetadataView {..}

all' :: Statement -> IO (Promise (D1ResultClass D1RowClass))
all' = js_all

raw :: Statement -> IO (Async (V.Vector (V.Vector D1ValueView)))
raw = deferWithM (V.mapM (fmap (V.map viewD1Value) . toVector) <=< toVector) <=< raw'

raw' :: Statement -> IO (Promise (SequenceClass (SequenceClass D1ValueClass)))
raw' = js_raw

run :: Statement -> IO (Async D1MetricsView)
run = deferWith viewMetrics <=< run'

run' :: Statement -> IO (Promise D1MetricsClass)
run' = js_run

rawWithColumns :: Statement -> IO (Async (V.Vector T.Text, V.Vector (V.Vector D1ValueView)))
rawWithColumns = deferWithM go <=< rawWithColumns'
  where
    go seqs = do
      vecs0 <- toVector seqs
      cols <-
        V.map (toText . unsafeCast @_ @USVStringClass)
          <$> toVector (V.head vecs0)
      rest <- mapM (fmap (V.map viewD1Value) . toVector) $ V.tail vecs0
      pure (cols, rest)

rawWithColumns' :: Statement -> IO (Promise (SequenceClass (SequenceClass D1ValueClass)))
rawWithColumns' = js_raw_with_cols

class ToD1Value a where
  {-# MINIMAL toD1Value | toD1ValueView #-}
  toD1Value :: a -> D1Value
  toD1Value = unviewD1Value . toD1ValueView
  toD1ValueView :: a -> D1ValueView
  toD1ValueView = viewD1Value . toD1Value

instance ToD1Value D1Value where
  toD1Value = id
  toD1ValueView = viewD1Value

instance ToD1Value D1ValueView where
  toD1Value = unviewD1Value
  toD1ValueView = id

class FromD1Value a where
  {-# MINIMAL parseD1Value | parseD1ValueView #-}
  parseD1Value :: D1Value -> Either String a
  {-# INLINE parseD1Value #-}
  parseD1Value = parseD1ValueView . viewD1Value
  parseD1ValueView :: D1ValueView -> Either String a
  {-# INLINE parseD1ValueView #-}
  parseD1ValueView = parseD1Value . unviewD1Value

instance FromD1Value D1Value where
  parseD1Value = Right
  parseD1ValueView = Right . unviewD1Value

instance FromD1Value D1ValueView where
  parseD1Value = Right . viewD1Value
  parseD1ValueView = Right

instance ToD1Value Bool where
  toD1Value =
    toD1Value . \case
      True -> 1 :: Int32
      False -> 0
  {-# INLINE toD1Value #-}
  toD1ValueView = D1Int . \case True -> 1; False -> 0
  {-# INLINE toD1ValueView #-}

instance FromD1Value Bool where
  parseD1ValueView = \case
    D1Int i -> Right $ i /= 0
    _ -> Left "Expected an integer"
  {-# INLINE parseD1ValueView #-}

instance FromD1Value Int32 where
  parseD1ValueView = \case
    D1Int x -> Right x
    _ -> Left "Expected an integer"
  {-# INLINE parseD1ValueView #-}
  parseD1Value p = case fromNullable $ js_decode_int p of
    Just x -> Right $ fromJSPrim x
    Nothing -> Left "Expected an integer"
  {-# INLINE parseD1Value #-}

instance ToD1Value Int32 where
  toD1Value = upcast . inject @_ @D1ValueAlts . toJSPrim
  {-# INLINE toD1Value #-}
  toD1ValueView = D1Int
  {-# INLINE toD1ValueView #-}

instance FromD1Value Int8 where
  parseD1ValueView = fmap (fromIntegral @Int32) . parseD1ValueView
  parseD1Value p = case fromNullable $ js_decode_int p of
    Just x -> Right $ fromIntegral $ fromJSPrim x
    Nothing -> Left "Expected an integer"
  {-# INLINE parseD1Value #-}

instance ToD1Value Int8 where
  toD1Value = toD1Value . fromIntegral @_ @Int32
  {-# INLINE toD1Value #-}
  toD1ValueView = toD1ValueView . fromIntegral @_ @Int32
  {-# INLINE toD1ValueView #-}

instance FromD1Value Int16 where
  parseD1ValueView = fmap (fromIntegral @Int32) . parseD1ValueView
  parseD1Value p = case fromNullable $ js_decode_int p of
    Just x -> Right $ fromIntegral $ fromJSPrim x
    Nothing -> Left "Expected an integer"
  {-# INLINE parseD1Value #-}

instance ToD1Value Int16 where
  toD1Value = toD1Value . fromIntegral @_ @Int32
  {-# INLINE toD1Value #-}
  toD1ValueView = toD1ValueView . fromIntegral @_ @Int32
  {-# INLINE toD1ValueView #-}

instance FromD1Value Word8 where
  parseD1ValueView = fmap (fromIntegral @Int32) . parseD1ValueView
  parseD1Value p = case fromNullable $ js_decode_int p of
    Just x -> Right $ fromIntegral $ fromJSPrim x
    Nothing -> Left "Expected an integer"
  {-# INLINE parseD1Value #-}

instance ToD1Value Word8 where
  toD1Value = toD1Value . fromIntegral @_ @Int32
  {-# INLINE toD1Value #-}
  toD1ValueView = toD1ValueView . fromIntegral @_ @Int32
  {-# INLINE toD1ValueView #-}

instance FromD1Value Word16 where
  parseD1ValueView = fmap (fromIntegral @Int32) . parseD1ValueView
  parseD1Value p = case fromNullable $ js_decode_int p of
    Just x -> Right $ fromIntegral $ fromJSPrim x
    Nothing -> Left "Expected an integer"
  {-# INLINE parseD1Value #-}

instance FromD1Value Word32 where
  parseD1ValueView = fmap (fromIntegral @Int32) . parseD1ValueView
  parseD1Value p = case fromNullable $ js_decode_int p of
    Just x -> Right $ fromIntegral $ fromJSPrim x
    Nothing -> Left "Expected an integer"
  {-# INLINE parseD1Value #-}

instance ToD1Value Word16 where
  toD1Value = toD1Value . fromIntegral @_ @Int32
  {-# INLINE toD1Value #-}
  toD1ValueView = toD1ValueView . fromIntegral @_ @Int32
  {-# INLINE toD1ValueView #-}

instance ToD1Value Word32 where
  toD1Value = toD1Value . fromIntegral @_ @Int32
  {-# INLINE toD1Value #-}
  toD1ValueView = toD1ValueView . fromIntegral @_ @Int32
  {-# INLINE toD1ValueView #-}

instance FromD1Value Double where
  parseD1ValueView = \case
    D1Real x -> Right x
    _ -> Left "Expected a real"
  {-# INLINE parseD1ValueView #-}
  parseD1Value p = case fromNullable $ js_decode_double p of
    Just x -> Right $ fromJSPrim x
    Nothing -> Left "Expected a double"
  {-# INLINE parseD1Value #-}

instance ToD1Value Double where
  toD1Value = upcast . inject @_ @D1ValueAlts . toJSPrim
  {-# INLINE toD1Value #-}
  toD1ValueView = D1Real
  {-# INLINE toD1ValueView #-}

instance FromD1Value Float where
  parseD1ValueView = fmap (realToFrac @Double) . parseD1ValueView
  {-# INLINE parseD1ValueView #-}
  parseD1Value = fmap realToFrac . parseD1Value @Double
  {-# INLINE parseD1Value #-}

instance ToD1Value Float where
  toD1Value = toD1Value . realToFrac @_ @Double
  {-# INLINE toD1Value #-}
  toD1ValueView = toD1ValueView . realToFrac @_ @Double
  {-# INLINE toD1ValueView #-}

instance ToD1Value T.Text where
  toD1Value = upcast . inject @USVStringClass @D1ValueAlts . fromText
  {-# INLINE toD1Value #-}
  toD1ValueView = D1Text
  {-# INLINE toD1ValueView #-}

instance FromD1Value T.Text where
  parseD1ValueView = \case
    D1Text txt -> Right txt
    _ -> Left "Expected a text"
  {-# INLINE parseD1ValueView #-}
  parseD1Value = nullable (Left "Expected a text") (Right . toText) . js_decode_string
  {-# INLINE parseD1Value #-}

instance ToD1Value LT.Text where
  toD1Value = toD1Value . LT.toStrict
  {-# INLINE toD1Value #-}
  toD1ValueView = toD1ValueView . LT.toStrict
  {-# INLINE toD1ValueView #-}

instance FromD1Value LT.Text where
  parseD1ValueView = fmap LT.fromStrict . parseD1ValueView
  {-# INLINE parseD1ValueView #-}
  parseD1Value = fmap LT.fromStrict . parseD1Value
  {-# INLINE parseD1Value #-}

instance ToD1Value BS.ByteString where
  toD1Value bs =
    upcast $
      inject @(JSByteArrayClass Word8) @D1ValueAlts $
        noinline $
          unsafePerformIO $
            useByteStringAsJSByteArray bs pure
  toD1ValueView = D1Blob
  {-# INLINE toD1ValueView #-}

instance ToD1Value String where
  toD1Value = toD1Value . T.pack
  {-# INLINE toD1Value #-}
  toD1ValueView = toD1ValueView . T.pack
  {-# INLINE toD1ValueView #-}

instance FromD1Value String where
  parseD1ValueView = fmap T.unpack . parseD1ValueView
  {-# INLINE parseD1ValueView #-}
  parseD1Value = fmap T.unpack . parseD1Value
  {-# INLINE parseD1Value #-}

instance FromD1Value BS.ByteString where
  parseD1ValueView = \case
    D1Blob bs -> Right bs
    _ -> Left "Expected a blob"
  {-# INLINE parseD1ValueView #-}
  parseD1Value =
    nullable
      (Left "Expected a blob")
      ( Right
          . Buffer.toByteString @Word8
          . noinline (unsafePerformIO . fromArrayBuffer)
      )
      . js_decode_arrbuf
  {-# INLINE parseD1Value #-}

instance FromD1Value LBS.ByteString where
  parseD1ValueView = fmap LBS.fromStrict . parseD1ValueView
  {-# INLINE parseD1ValueView #-}
  parseD1Value = fmap LBS.fromStrict . parseD1Value
  {-# INLINE parseD1Value #-}

instance (ToD1Value a) => ToD1Value (Maybe a) where
  toD1Value = maybe (upcast jsNull) toD1Value
  {-# INLINE toD1Value #-}
  toD1ValueView = maybe D1Null toD1ValueView
  {-# INLINE toD1ValueView #-}

instance (FromD1Value a) => FromD1Value (Maybe a) where
  parseD1ValueView = \case
    D1Null -> Right Nothing
    x -> fmap Just $ parseD1ValueView x
  {-# INLINE parseD1ValueView #-}
  parseD1Value = nullable (Right Nothing) (fmap Just . parseD1Value . upcast)
  {-# INLINE parseD1Value #-}

instance ToD1Value UTCTime where
  toD1Value = toD1Value . iso8601Show
  {-# INLINE toD1Value #-}
  toD1ValueView = toD1ValueView . iso8601Show
  {-# INLINE toD1ValueView #-}

instance FromD1Value UTCTime where
  parseD1Value = runTry . iso8601ParseM <=< parseD1Value
  {-# INLINE parseD1Value #-}
  parseD1ValueView = runTry . iso8601ParseM <=< parseD1ValueView
  {-# INLINE parseD1ValueView #-}

runTry :: Try a -> Either String a
runTry = (.runTry)

newtype Try a = Try {runTry :: Either String a}
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (Functor, Applicative, Monad)

instance MonadFail Try where
  fail = Try . Left
  {-# INLINE fail #-}

type D1MetadataFields =
  '[ '("duration", JSPrimClass Double)
   , '("rows_read", NullableClass (JSPrimClass Word32))
   , '("rows_written", NullableClass (JSPrimClass Word32))
   ]

type D1MetadataClass = JSDictionaryClass D1MetadataFields

type D1Metadata = JSObject D1MetadataClass

type D1ResultFields cls =
  '[ '("results", SequenceClass cls)
   , '("success", JSPrimClass Bool)
   , '("meta", D1MetadataClass)
   ]

type D1ResultClass cls = JSDictionaryClass (D1ResultFields cls)

type D1Result cls = JSObject (D1ResultClass cls)

data D1ResultView a = D1ResultView
  { results :: V.Vector a
  , success :: Bool
  , meta :: D1MetadataView
  }
  deriving (Show, Eq, Ord, Generic, Functor, Foldable, Traversable)

data D1MetadataView = D1MetadataView
  { duration :: Double
  , rowsRead :: Maybe Word32
  , rowsWritten :: Maybe Word32
  }
  deriving (Show, Eq, Ord, Generic)

type D1ExecResultFields =
  '[ '("count", JSPrimClass Word32)
   , '("duration", JSPrimClass Double)
   ]

data D1ExecResultView = D1ExecResult {count :: !Word32, duration :: !Double}
  deriving (Show, Eq, Ord, Generic)

viewD1ExecResult :: D1ExecResult -> D1ExecResultView
viewD1ExecResult res = unsafeDupablePerformIO do
  res' <- reifyDictionary res
  pure D1ExecResult {count = fromJSPrim res'.count, duration = fromJSPrim res'.duration}

type D1ExecResultClass = JSDictionaryClass D1ExecResultFields

type D1ExecResult = JSObject D1ExecResultClass

type D1RowClass = JSRecordClass USVStringClass D1ValueClass

type D1Row = JSObject D1RowClass

newtype D1RowView = D1RowView {getD1RowView :: Map T.Text D1ValueView}
  deriving (Show, Eq, Ord, Generic)

first :: Statement -> IO (Async (Maybe D1RowView))
first = deferWith (nullable Nothing (Just . viewD1Row)) <=< first'

first' :: Statement -> IO (Promise (NullableClass D1RowClass))
first' = js_first

firstColumns ::
  Statement ->
  V.Vector T.Text ->
  IO (Async (Maybe (V.Vector D1ValueView)))
firstColumns stmt = deferWithM go <=< firstColumns' stmt . toSequence . V.map fromText
  where
    go = nullable (pure Nothing) (fmap (Just . V.map viewD1Value) . toVector)

firstColumns' ::
  Statement ->
  Sequence USVStringClass ->
  IO (Promise (NullableClass (SequenceClass D1ValueClass)))
firstColumns' = js_first_with_cols

batch :: D1 -> V.Vector Statement -> IO (Async (V.Vector (D1ResultView D1RowView)))
batch d1 stmts =
  deferWithM (fmap (V.map (fromResults viewD1Row)) . toVector)
    =<< js_batch d1 (toSequence stmts)

exec :: D1 -> T.Text -> IO (Async D1ExecResultView)
exec d1 rawQry = deferWith viewD1ExecResult =<< exec' d1 (fromText rawQry)

exec' :: D1 -> USVString -> IO (Promise D1ExecResultClass)
exec' = js_exec

data D1Error
  = D1Error {d1ErrorMessage :: !T.Text}
  | D1TypeError {d1ErrorMessage :: !T.Text}
  | D1ColumnNotFound {d1ErrorMessage :: !T.Text}
  | D1DumpError {d1ErrorMessage :: !T.Text}
  | D1ExecError {d1ErrorMessage :: !T.Text}
  deriving (Show, Eq, Ord, Generic)

instance Exception D1Error where
  toException err =
    toException $ JSException $ unJSObject $ fromText @USVStringClass err.d1ErrorMessage
  fromException exc = case fromException exc of
    Just (JSException obj) ->
      let msg = toText $ js_exc_msg obj
       in case CI.mk $ T.takeWhile (/= ':') msg of
            "D1_ERROR" -> Just $ D1Error msg
            "D1_TYPE_ERROR" -> Just $ D1TypeError msg
            "D1_COLUMN_NOTFOUND" -> Just $ D1ColumnNotFound msg
            "D1_DUMP_ERROR" -> Just $ D1DumpError msg
            "D1_EXEC_ERROR" -> Just $ D1ExecError msg
            _ -> Nothing
    _ -> Nothing

foreign import javascript unsafe "$1.prepare($2)"
  js_d1_prepare :: D1 -> JSString -> IO PreparedStatement

foreign import javascript unsafe "$1.bind(... $2)"
  js_d1_bind :: PreparedStatement -> Sequence D1ValueClass -> IO Statement

foreign import javascript unsafe "$1 === null"
  js_is_null :: JSObject c -> Bool

foreign import javascript unsafe "typeof $1 === 'number'"
  js_is_number :: JSObject c -> Bool

foreign import javascript unsafe "Number.isInteger($1)"
  js_is_integer :: JSObject c -> Bool

foreign import javascript unsafe "if (typeof $1 === 'number' && Number.isInteger($1)) { return $1; } else { return null; }"
  js_decode_int :: JSObject c -> Nullable (JSPrimClass Int32)

foreign import javascript unsafe "if (typeof $1 === 'number') { if (Number.isInteger($1)) { return null } else { return $1; } } else { return null; }"
  js_decode_double :: JSObject c -> Nullable (JSPrimClass Double)

foreign import javascript unsafe "if (typeof $1 === 'string') { return $1; } else { return null; }"
  js_decode_string :: JSObject c -> Nullable USVStringClass

foreign import javascript unsafe "if ($1 instanceof ArrayBuffer) { return $1; } else { return null; }"
  js_decode_arrbuf :: JSObject c -> Nullable ArrayBufferClass

foreign import javascript unsafe "typeof $1 === 'string'"
  js_is_string :: JSObject c -> Bool

foreign import javascript safe "$1.all()"
  js_all :: Statement -> IO (Promise (D1ResultClass D1RowClass))

foreign import javascript safe "$1.raw()"
  js_raw :: Statement -> IO (Promise (SequenceClass (SequenceClass D1ValueClass)))

foreign import javascript safe "$1.raw({columnNames: true})"
  js_raw_with_cols :: Statement -> IO (Promise (SequenceClass (SequenceClass D1ValueClass)))

foreign import javascript safe "$1.first()"
  js_first :: Statement -> IO (Promise (NullableClass D1RowClass))

foreign import javascript safe "$1.first(... $2)"
  js_first_with_cols ::
    Statement ->
    Sequence USVStringClass ->
    IO (Promise (NullableClass (SequenceClass D1ValueClass)))

foreign import javascript safe "$1.run()"
  js_run :: Statement -> IO (Promise D1MetricsClass)

foreign import javascript safe "$1.exec()"
  js_exec :: D1 -> USVString -> IO (Promise D1ExecResultClass)

foreign import javascript safe "$1.batch($2)"
  js_batch :: D1 -> Sequence StatementClass -> IO (Promise (SequenceClass (D1ResultClass D1RowClass)))

foreign import javascript unsafe "$3[$1] = $2; return $3"
  js_partial_set :: JSString -> D1Value -> PartialRow %1 -> PartialRow

foreign import javascript unsafe "new Object()"
  js_new_row :: PartialRow

foreign import javascript unsafe "$2[$1]"
  js_row_get :: JSString -> D1Row -> D1Value

foreign import javascript unsafe "$1.message"
  js_exc_msg :: JSVal -> USVString
