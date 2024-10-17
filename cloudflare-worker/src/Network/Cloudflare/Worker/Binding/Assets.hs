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
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Network.Cloudflare.Worker.Binding.Assets (Assets, AssetsClass, fetch) where

import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.URL (URLClass)
import Network.Cloudflare.Worker.Request (WorkerRequestClass)
import Network.Cloudflare.Worker.Response (WorkerResponseClass)
import Prelude hiding (all, head)

type data AssetsClass :: Prototype

type instance SuperclassOf AssetsClass = 'Nothing

type Assets = JSObject AssetsClass

foreign import javascript safe "$1.fetch($2)"
  fetch :: Assets -> Union '[WorkerRequestClass, USVStringClass, URLClass] -> IO (Promise WorkerResponseClass)
