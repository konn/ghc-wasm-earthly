{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}

module Steward.Demo.Fib.Types (FibResult (..), FibEndpoints (..)) where

import Data.Aeson
import GHC.Generics (Generic)
import Steward.Types

data FibResult = FibResult {input, result :: !Int}
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

data FibEndpoints mode = FibEndpoints
  { index :: mode ::: Get PlainText
  , fib :: mode ::: "fib" /> Int /> Get (JSON FibResult)
  , random :: mode ::: "random" /> Get (JSON Int)
  }
  deriving (Generic)
  deriving anyclass (HasHandler m, HasClient)
