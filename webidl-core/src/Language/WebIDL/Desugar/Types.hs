{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Language.WebIDL.Desugar.Types (
  Definitions,
  Definitions' (..),
  Interface,
  Interface' (..),
  ArgumentList (..),
  Namespace,
  Namespace' (..),
  Mixin,
  Mixin' (..),
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
import Barbies
import Control.Exception (Exception)
import Control.Monad.State (MonadState)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict
import Data.DList (DList)
import Data.DList qualified as DL
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Monoidal.Strict (MonoidalMap)
import Data.Map.Strict (Map)
import Data.Monoid
import Data.Text (Text)
import Data.Vector qualified as V
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

type Interface = Interface' V.Vector

data Interface' h = Interface
  { parent :: !(First Identifier)
  , constructors :: !(h (Attributed ArgumentList))
  , constants :: !(h (Attributed Const))
  , operations :: !(h (Attributed Operation))
  , stringifiers :: !(h (Attributed Stringifier))
  , attributes :: !(h (Attributed (Access, Attribute)))
  , inheritedAttributes :: !(h (Attributed Attribute))
  , iterables :: !(h (Attributed Iterable))
  , maplikes :: !(h (Attributed (Access, Maplike)))
  , setlikes :: !(h (Attributed (Access, Setlike)))
  , asyncIterables :: !(h (Attributed AsyncIterable))
  , staticAttributes :: !(h (Attributed (Access, Attribute)))
  , staticOperations :: !(h (Attributed RegularOperation))
  }
  deriving (Generic)
  deriving anyclass (FunctorB, ConstraintsB)

deriving via Generically (Interface' DList) instance Semigroup (Interface' DList)

deriving via Generically (Interface' DList) instance Monoid (Interface' DList)

deriving instance (AllBF Show h Interface') => Show (Interface' h)

deriving instance (AllBF Eq h Interface') => Eq (Interface' h)

deriving instance (AllBF Ord h Interface') => Ord (Interface' h)

type Namespace = Namespace' V.Vector

data Namespace' h = Namespace
  { operations :: !(h (Attributed RegularOperation))
  , readOnlyAttributes :: !(h (Attributed Attribute))
  , constants :: !(h (Attributed Const))
  }
  deriving (Generic)
  deriving anyclass (FunctorB, ConstraintsB)

deriving instance (AllBF Show h Namespace') => Show (Namespace' h)

deriving instance (AllBF Eq h Namespace') => Eq (Namespace' h)

deriving instance (AllBF Ord h Namespace') => Ord (Namespace' h)

deriving via
  Generically (Namespace' DList)
  instance
    Semigroup (Namespace' DList)

deriving via
  Generically (Namespace' DList)
  instance
    Monoid (Namespace' DList)

type Mixin = Mixin' DList

data Mixin' h = Mixin
  { constants :: !(h (Attributed Const))
  , operations :: !(h (Attributed RegularOperation))
  , stringifiers :: !(h (Attributed Stringifier))
  , attributes :: !(h (Attributed (Access, Attribute)))
  }
  deriving (Generic)
  deriving anyclass (FunctorB, ConstraintsB)

deriving instance (AllBF Show h Mixin') => Show (Mixin' h)

deriving instance (AllBF Eq h Mixin') => Eq (Mixin' h)

deriving instance (AllBF Ord h Mixin') => Ord (Mixin' h)

deriving via
  Generically (Mixin' DList)
  instance
    Semigroup (Mixin' DList)

deriving via
  Generically (Mixin' DList)
  instance
    Monoid (Mixin' DList)

data Typedef = Typedef
  deriving (Show, Eq, Generic)
  deriving (Semigroup, Monoid) via Generically Typedef

newtype Enumeration = Enumeration (NonEmpty Text)
  deriving (Show, Eq, Ord, Generic)

data Dictionary = Dictionary
  { requiredMembers :: !(Map Identifier (Attributed IDLType))
  , optionalMembers :: !(Map Identifier (Attributed (IDLType, Maybe DefaultValue)))
  }
  deriving (Show, Eq, Ord, Generic)
  deriving (Semigroup, Monoid) via Generically Dictionary

data Definitions' p = Definitions
  { interfaces :: !(MonoidalMap Identifier (Attributed (Interface' p)))
  , namespaces :: !(MonoidalMap Identifier (Attributed (Namespace' p)))
  , mixins :: !(MonoidalMap Identifier (Attributed (Mixin' p)))
  , typedefs :: !(Map Identifier (Attributed (Attributed IDLType)))
  , enums :: !(Map Identifier (Attributed Enumeration))
  , dictionaries :: !(MonoidalMap Identifier (Attributed Dictionary))
  , callbackFunctions :: !(Map Identifier (Attributed CallbackFunction))
  , callbackInterfaces :: !(Map Identifier (Attributed CallbackInterface))
  , interfaceInheritance :: !(AM.AdjacencyMap Identifier)
  , dictionaryInheritance :: !(AM.AdjacencyMap Identifier)
  }
  deriving (Generic)
  deriving anyclass (FunctorB)

deriving instance
  ( AllBF Show h Mixin'
  , AllBF Show h Interface'
  ) =>
  Show (Definitions' h)

deriving instance
  ( AllBF Eq h Mixin'
  , AllBF Eq h Interface'
  ) =>
  Eq (Definitions' h)

deriving instance
  ( AllBF Ord h Mixin'
  , AllBF Ord h Interface'
  ) =>
  Ord (Definitions' h)

deriving via Generically (Definitions' DList) instance Semigroup (Definitions' DList)

deriving via Generically (Definitions' DList) instance Monoid (Definitions' DList)

data DesugarError
  = InterfaceAlreadyDefined !Identifier
  | MixinAlreadyDefined !Identifier
  | DictionaryAlreadyDefined !Identifier
  | NoCompleteEntityFound !IDLFragment
  | CyclicInheritance !(NonEmpty Text)
  | MixinNotFound !Identifier
  deriving (Show, Eq, Generic)
  deriving anyclass (Exception)

type Definitions = Definitions' V.Vector

newtype Desugarer a = Desugarer {unDesugarer :: StateT (Definitions' DList) (Either DesugarError) a}
  deriving newtype (Functor, Applicative, Monad, MonadState (Definitions' DList))
  deriving (Semigroup, Monoid) via Ap Desugarer a

runDesugarer :: Desugarer () -> Either DesugarError Definitions
runDesugarer = fmap finalise . flip execStateT mempty . (.unDesugarer)

finalise :: Definitions' DList -> Definitions
finalise = bmap $ V.fromList . DL.toList

throw :: DesugarError -> Desugarer a
throw = Desugarer . lift . Left

mixinToInterface :: Mixin' DList -> Interface' DList
mixinToInterface Mixin {..} =
  (mempty @(Interface' DList))
    { constants
    , operations = fmap (fmap $ Operation Nothing) operations
    , stringifiers
    , attributes
    }