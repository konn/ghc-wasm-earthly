{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnliftedDatatypes #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module GHC.Wasm.Object.Builtins.Date (
  Date,
  DateClass,
  getCurrentDate,
  parse,
  toUnixTime,
  fromUnixTime,
  toUTCTime,
  getUTCDate,
  getUTCFullYear,
  getUTCMonth,
  getUTCHours,
  getUTCMinutes,
  getUTCSeconds,
  getUTCMilliseconds,
) where

import Data.Ratio ((%))
import qualified Data.Time as Time
import qualified Data.Time.Clock.POSIX as Time
import GHC.Wasm.Object.Core
import GHC.Wasm.Prim

-- TODO: implement the 'Date' API and conversion between UTCTime

type data DateClass :: Prototype

type Date = JSObject DateClass

type instance SuperclassOf DateClass = 'Nothing

toUnixTime :: Date -> IO Time.POSIXTime
toUnixTime = fmap (fromRational . (% 1_000) . fromIntegral) . js_get_time

-- | NOTE: Only precision up to milliseconds is preserved
fromUnixTime :: Time.POSIXTime -> IO Date
fromUnixTime = js_new_from_POSIX . floor . (* 1_000) . toRational

toUTCTime :: Date -> IO Time.UTCTime
toUTCTime = fmap Time.posixSecondsToUTCTime . toUnixTime

foreign import javascript unsafe "new Date()"
  getCurrentDate :: IO Date

foreign import javascript unsafe "Date.parse($1)"
  parse :: JSString -> Date

foreign import javascript unsafe "new Date($1)"
  js_new_from_POSIX :: Int -> IO Date

foreign import javascript unsafe "$1.getTime()"
  js_get_time :: Date -> IO Int

foreign import javascript unsafe "$1.getUTCFullYear()"
  getUTCFullYear :: Date -> IO Int

foreign import javascript unsafe "$1.getUTCMonth()"
  getUTCMonth :: Date -> IO Int

foreign import javascript unsafe "$1.getUTCDate()"
  getUTCDate :: Date -> IO Int

foreign import javascript unsafe "$1.getUTCHours()"
  getUTCHours :: Date -> IO Int

foreign import javascript unsafe "$1.getUTCMinutes()"
  getUTCMinutes :: Date -> IO Int

foreign import javascript unsafe "$1.getUTCSeconds()"
  getUTCSeconds :: Date -> IO Int

foreign import javascript unsafe "$1.getUTCMilliseconds()"
  getUTCMilliseconds :: Date -> IO Int
