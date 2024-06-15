{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Language.WebIDL.Desugar.Types (
  Definitions (..),
  Interface (..),
  ArgumentList (..),
  Namespace (..),
  Mixin (..),
  Identifier,
  Typedef (..),
  IDLType (..),
  Const (..),
  OperationName (..),
  Operation (..),
  RegularOperation (..),
  Argument (..),
  OptionalArgument (..),
  Ellipsis (..),
  Stringifier (..),
  Maplike (..),
  Special (..),
  Setlike (..),
  Iterable (..),
  Access (..),
  AsyncIterable (..),
  mixinToInterface,
  Enumeration (..),
  Dictionary (..),
  CallbackFunction (..),
  CallbackInterface (..),
  Desugarer (),
  runDesugarer,
  throw,
  DesugarError (..),
  ExtendedAttribute (..),
  Attributed (..),
  DefaultValue (..),
) where

import Algebra.Graph.AdjacencyMap qualified as AM
import Control.Exception (Exception)
import Control.Monad.State (MonadState)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict
import Data.DList (DList)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Monoidal.Strict (MonoidalMap)
import Data.Map.Strict (Map)
import Data.Monoid
import Data.Text (Text)
import GHC.Generics
import Language.WebIDL.AST.Types (
  Access (..),
  Argument (..),
  ArgumentList (..),
  AsyncIterable (..),
  Attribute,
  Attributed (..),
  CallbackFunction (..),
  CallbackInterface (..),
  Const (..),
  DefaultValue (..),
  Ellipsis (..),
  ExtendedAttribute (..),
  IDLFragment,
  IDLType (..),
  Identifier,
  Iterable (..),
  Maplike (..),
  Operation (..),
  OperationName (..),
  OptionalArgument (..),
  RegularOperation (..),
  Setlike (..),
  Special (..),
  Stringifier (..),
 )

data Interface = Interface
  { parent :: !(First Identifier)
  , constructors :: !(DList (Attributed ArgumentList))
  , constants :: !(DList (Attributed Const))
  , operations :: !(DList (Attributed Operation))
  , stringifiers :: !(DList (Attributed Stringifier))
  , attributes :: !(DList (Attributed (Access, Attribute)))
  , inheritedAttributes :: !(DList (Attributed Attribute))
  , iterables :: !(DList (Attributed Iterable))
  , maplikes :: !(DList (Attributed (Access, Maplike)))
  , setlikes :: !(DList (Attributed (Access, Setlike)))
  , asyncIterables :: !(DList (Attributed AsyncIterable))
  , staticAttributes :: !(DList (Attributed (Access, Attribute)))
  , staticOperations :: !(DList (Attributed RegularOperation))
  }
  deriving (Show, Eq, Generic)
  deriving (Semigroup, Monoid) via Generically Interface

data Namespace = Namespace
  { operations :: !(DList (Attributed RegularOperation))
  , readOnlyAttributes :: !(DList (Attributed Attribute))
  , constants :: !(DList (Attributed Const))
  }
  deriving (Show, Eq, Generic)
  deriving (Semigroup, Monoid) via Generically Namespace

data Mixin = Mixin
  { constants :: !(DList (Attributed Const))
  , operations :: !(DList (Attributed RegularOperation))
  , stringifiers :: !(DList (Attributed Stringifier))
  , attributes :: !(DList (Attributed (Access, Attribute)))
  }
  deriving (Show, Eq, Generic)
  deriving (Semigroup, Monoid) via Generically Mixin

data Typedef = Typedef
  deriving (Show, Eq, Generic)
  deriving (Semigroup, Monoid) via Generically Typedef

newtype Enumeration = Enumeration (NonEmpty Text)
  deriving (Show, Eq, Generic)

data Dictionary = Dictionary
  { requiredMembers :: !(Map Identifier (Attributed IDLType))
  , optionalMembers :: !(Map Identifier (Attributed (IDLType, Maybe DefaultValue)))
  }
  deriving (Show, Eq, Generic)
  deriving (Semigroup, Monoid) via Generically Dictionary

data Definitions = Definitions
  { interfaces :: !(MonoidalMap Identifier (Attributed Interface))
  , namespaces :: !(MonoidalMap Identifier (Attributed Namespace))
  , mixins :: !(MonoidalMap Identifier (Attributed Mixin))
  , typedefs :: !(Map Identifier (Attributed (Attributed IDLType)))
  , enums :: !(Map Identifier (Attributed Enumeration))
  , dictionaries :: !(MonoidalMap Identifier (Attributed Dictionary))
  , callbackFunctions :: !(Map Identifier (Attributed CallbackFunction))
  , callbackInterfaces :: !(Map Identifier (Attributed CallbackInterface))
  , interfaceInheritance :: !(AM.AdjacencyMap Identifier)
  , dictionaryInheritance :: !(AM.AdjacencyMap Identifier)
  }
  deriving (Show, Eq, Generic)
  deriving (Semigroup, Monoid) via Generically Definitions

data DesugarError
  = InterfaceAlreadyDefined !Identifier
  | MixinAlreadyDefined !Identifier
  | DictionaryAlreadyDefined !Identifier
  | NoCompleteEntityFound !IDLFragment
  | CyclicInheritance !(NonEmpty Text)
  | MixinNotFound !Identifier
  deriving (Show, Eq, Generic)
  deriving anyclass (Exception)

newtype Desugarer a = Desugarer {unDesugarer :: StateT Definitions (Either DesugarError) a}
  deriving newtype (Functor, Applicative, Monad, MonadState Definitions)
  deriving (Semigroup, Monoid) via Ap Desugarer a

runDesugarer :: Desugarer () -> Either DesugarError Definitions
runDesugarer = flip execStateT mempty . (.unDesugarer)

throw :: DesugarError -> Desugarer a
throw = Desugarer . lift . Left

mixinToInterface :: Mixin -> Interface
mixinToInterface Mixin {..} =
  mempty
    { constants
    , operations = fmap (fmap $ Operation Nothing) operations
    , stringifiers
    , attributes
    }