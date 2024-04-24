{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Wasm.Data.Unrestricted.Linear.Monadic (
  ConsumableM (..),
  DupableM (..),
  dupM,
  MovableM (..),
  Purely (..),
) where

import qualified GHC.IO as System
import GHC.Wasm.Prim
import qualified Wasm.Control.Functor.Linear as Control
import Wasm.Data.Function.Linear
import Wasm.Data.Unrestricted.Linear
import Wasm.System.IO.Linear (MonadIO (..))
import qualified Wasm.Unsafe.Linear as Unsafe
import Prelude (error)
import qualified Prelude

newtype Purely a = Purely a
  deriving newtype (Consumable, Dupable, Movable)

class ConsumableM m a where
  consumeM :: a %1 -> m ()

class (ConsumableM m a) => DupableM m a where
  dup2M :: a %1 -> m (a, a)

dupM :: (DupableM m a) => a %1 -> m (a, a)
{-# INLINE dupM #-}
dupM = dup2M

class (DupableM m a) => MovableM m a where
  moveM :: a %1 -> m (Ur a)

instance (Consumable a, Control.Applicative m) => ConsumableM m (Purely a) where
  consumeM = Control.pure . consume
  {-# INLINE consumeM #-}

instance (Dupable a, Control.Applicative m) => DupableM m (Purely a) where
  dup2M = Control.pure . dup2
  {-# INLINE dup2M #-}

instance (Movable a, Control.Applicative m) => MovableM m (Purely a) where
  moveM = Control.pure . move
  {-# INLINE moveM #-}

instance (MonadIO m) => ConsumableM m JSVal where
  consumeM = liftIO . Unsafe.coerce . Unsafe.toLinear freeJSVal
  {-# INLINE consumeM #-}

deriving newtype instance (MonadIO m) => ConsumableM m JSString

foreign import javascript unsafe "String($1)"
  js_clone_string :: JSString -> System.IO JSString

instance (MonadIO m) => DupableM m JSString where
  dup2M =
    liftIO . Unsafe.coerce . Unsafe.toLinear \s -> do
      b <- js_clone_string s
      Prelude.pure (s, b)
  {-# INLINE dup2M #-}
