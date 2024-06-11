{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoFieldSelectors #-}

module Language.WebIDL.Desugar where

import Algebra.Graph.AdjacencyMap qualified as AM
import Algebra.Graph.AdjacencyMap.Algorithm qualified as AM
import Control.Lens
import Control.Lens.Extras (is)
import Control.Monad (forM_, when)
import Data.Bifunctor qualified as Bi
import Data.Coerce (coerce)
import Data.DList (DList)
import Data.DList qualified as DL
import Data.Functor.Compose
import Data.Generics.Labels ()
import Data.Maybe (isJust)
import Data.Vector qualified as V
import GHC.Generics (Generic, Generically (..))
import Language.WebIDL.AST.Types (IDLFragment)
import Language.WebIDL.AST.Types qualified as AST
import Language.WebIDL.Desugar.Types

data Completes
  = Completes
  { interfaces :: !(DList (Identifier, Attributed (AST.Interface AST.Complete)))
  , namespaces :: !(DList (Identifier, Attributed AST.Namespace))
  , mixins :: !(DList (Identifier, Attributed AST.Mixin))
  , dictionaries :: !(DList (Identifier, Attributed AST.Dictionary))
  , callbackFunctions :: !(DList (Identifier, Attributed AST.CallbackFunction))
  , callbackInterfaces :: !(DList (Identifier, Attributed AST.CallbackInterface))
  , typedefs :: !(DList (Identifier, Attributed AST.Typedef))
  , enumerations :: !(DList (Identifier, Attributed AST.Enum_))
  }
  deriving (Show, Eq, Ord, Generic)
  deriving (Semigroup, Monoid) via Generically Completes

data Partials
  = Partials
  { interfaces :: !(DList (Identifier, Attributed (AST.Interface AST.Partial)))
  , inclusions :: !(DList (Identifier, AST.IncludesStatement))
  , namespaces :: !(DList (Identifier, Attributed AST.Namespace))
  , dictionaries :: !(DList (Identifier, Attributed AST.Dictionary))
  , mixins :: !(DList (Identifier, Attributed AST.Mixin))
  }
  deriving (Show, Eq, Ord, Generic)
  deriving (Semigroup, Monoid) via Generically Partials

desugar :: forall t. (Foldable t) => t IDLFragment -> Either DesugarError Definitions
desugar frags = runDesugarer $ do
  let (comps, partials) =
        foldMapOf
          (folded . #_IDLFragment . folded)
          ( \Attributed {..} ->
              case entry of
                AST.InterfaceD n i ->
                  ( mempty
                      & #interfaces
                        .~ DL.singleton (n, Attributed {entry = i, ..})
                  , mempty
                  )
                AST.PartialInterfaceD n i ->
                  ( mempty
                  , mempty
                      & #interfaces
                        .~ DL.singleton (n, Attributed {entry = i, ..})
                  )
                AST.InterfaceMixinD n i ->
                  ( mempty
                      & #mixins
                        .~ DL.singleton (n, Attributed {entry = i, ..})
                  , mempty
                  )
                AST.PartialMixinD n i ->
                  ( mempty
                  , mempty
                      & #mixins
                        .~ DL.singleton (n, Attributed {entry = i, ..})
                  )
                AST.NamespaceD n i ->
                  ( mempty
                      & #namespaces
                        .~ DL.singleton (n, Attributed {entry = i, ..})
                  , mempty
                  )
                AST.PartialNamespaceD n i ->
                  ( mempty
                  , mempty
                      & #namespaces
                        .~ DL.singleton (n, Attributed {entry = i, ..})
                  )
                AST.DictionaryD n i ->
                  ( mempty
                      & #dictionaries
                        .~ DL.singleton (n, Attributed {entry = i, ..})
                  , mempty
                  )
                AST.PartialDictionaryD n i ->
                  ( mempty
                  , mempty
                      & #dictionaries
                        .~ DL.singleton (n, Attributed {entry = i, ..})
                  )
                AST.CallbackFunctionD n i ->
                  ( mempty
                      & #callbackFunctions
                        .~ DL.singleton (n, Attributed {entry = i, ..})
                  , mempty
                  )
                AST.CallbackInterfaceD n i ->
                  ( mempty
                      & #callbackInterfaces
                        .~ DL.singleton (n, Attributed {entry = i, ..})
                  , mempty
                  )
                AST.TypedefD n i ->
                  ( mempty
                      & #typedefs
                        .~ DL.singleton (n, Attributed {entry = i, ..})
                  , mempty
                  )
                AST.EnumD n i ->
                  ( mempty
                      & #enumerations
                        .~ DL.singleton (n, Attributed {entry = i, ..})
                  , mempty
                  )
                AST.IncludesStatementD n i ->
                  ( mempty
                  , mempty & #inclusions .~ DL.singleton (n, i)
                  )
          )
          frags
  resolveCompletes comps
  resolvePartials partials

resolvePartials :: Partials -> Desugarer ()
resolvePartials = _

resolveCompletes :: Completes -> Desugarer ()
resolveCompletes Completes {..} = do
  forM_ interfaces \(n, Attributed {..}) -> do
    registerInterface n attributes entry
  eith <- uses #inheritance AM.topSort
  case eith of
    Left cyc -> throw $ CyclicInheritance cyc
    Right {} -> pure ()
  pure ()

registerInterface ::
  Identifier ->
  V.Vector ExtendedAttribute ->
  AST.Interface AST.Complete ->
  Desugarer ()
registerInterface name atts (AST.Interface inh body) = do
  old <-
    #interfaces . at name
      <<?= Attributed {entry = desugarInterface body, attributes = atts}
  when (isJust old) $ throw $ InterfaceAlreadyDefined name
  case inh of
    AST.NoInheritance -> pure ()
    AST.Inherits super ->
      #inheritance <>= AM.edge name super

desugarInterface :: V.Vector (Attributed (AST.InterfaceMember p)) -> Interface
desugarInterface = _
