{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Language.WebIDL.Desugar.Types (
  Definitions (..),
  Interface (..),
  Namespace (..),
  Mixin (..),
  Identifier,
  Typedef (..),
  Enumeration (..),
  Dictionary (..),
  CallbackFunction (..),
  CallbackInterface (..),
  Args (..),
  Desugarer (),
  runDesugarer,
  throw,
  DesugarError (..),
  ExtendedAttribute (..),
  Attributed (..),
) where

import Algebra.Graph.AdjacencyMap qualified as AM
import Control.Exception (Exception)
import Control.Monad.State (MonadState)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict
import Data.DList (DList)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Monoidal.Strict (MonoidalMap)
import Data.Monoid
import Data.Text (Text)
import GHC.Generics
import Language.WebIDL.AST.Types (Attributed (..), ExtendedAttribute (..), IDLFragment, Identifier)

data Interface = Interface {constructors :: !(DList Args)}
  deriving (Show, Eq, Generic)
  deriving (Semigroup, Monoid) via Generically Interface

data Namespace = Namespace
  deriving (Show, Eq, Generic)
  deriving (Semigroup, Monoid) via Generically Namespace

data Mixin = Mixin
  deriving (Show, Eq, Generic)
  deriving (Semigroup, Monoid) via Generically Mixin

data Typedef = Typedef
  deriving (Show, Eq, Generic)
  deriving (Semigroup, Monoid) via Generically Typedef

data Enumeration = Enumeration
  deriving (Show, Eq, Generic)
  deriving (Semigroup, Monoid) via Generically Enumeration

data Dictionary = Dictionary
  deriving (Show, Eq, Generic)
  deriving (Semigroup, Monoid) via Generically Dictionary

data CallbackFunction = CallbackFunction
  deriving (Show, Eq, Generic)
  deriving (Semigroup, Monoid) via Generically CallbackFunction

data CallbackInterface = CallbackInterface
  deriving (Show, Eq, Generic)
  deriving (Semigroup, Monoid) via Generically CallbackInterface

data Definitions = Definitions
  { interfaces :: !(MonoidalMap Identifier (Attributed Interface))
  , namespaces :: !(MonoidalMap Identifier (Attributed Namespace))
  , mixins :: !(MonoidalMap Identifier (Attributed Mixin))
  , typedefs :: !(MonoidalMap Identifier (Attributed Typedef))
  , enums :: !(MonoidalMap Identifier (Attributed Enumeration))
  , dictionaries :: !(MonoidalMap Identifier (Attributed Dictionary))
  , callbackFunctions :: !(MonoidalMap Identifier (Attributed CallbackFunction))
  , callbackInterfaces :: !(MonoidalMap Identifier (Attributed CallbackInterface))
  , inheritance :: !(AM.AdjacencyMap Identifier)
  }
  deriving (Show, Eq, Generic)
  deriving (Semigroup, Monoid) via Generically Definitions

data DesugarError
  = InterfaceAlreadyDefined !Identifier
  | NoCompleteEntityFound !IDLFragment
  | CyclicInheritance !(NonEmpty Text)
  deriving (Show, Eq, Generic)
  deriving anyclass (Exception)

newtype Desugarer a = Desugarer {unDesugarer :: StateT Definitions (Either DesugarError) a}
  deriving newtype (Functor, Applicative, Monad, MonadState Definitions)
  deriving (Semigroup, Monoid) via Ap Desugarer a

runDesugarer :: Desugarer () -> Either DesugarError Definitions
runDesugarer = flip execStateT mempty . (.unDesugarer)

throw :: DesugarError -> Desugarer a
throw = Desugarer . lift . Left
