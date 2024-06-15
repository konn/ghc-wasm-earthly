{-# LANGUAGE ApplicativeDo #-}
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
import Control.Foldl qualified as L
import Control.Lens (
  At (at),
  foldMapOf,
  folded,
  uses,
  (&),
  (.~),
  (<<?=),
  (<>=),
 )
import Control.Lens qualified as Lens
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
import Language.WebIDL.AST.Types (IDLFragment, SPartiality (..), sPartiality)
import Language.WebIDL.AST.Types qualified as AST
import Language.WebIDL.Desugar.Types

data Completes
  = Completes
  { interfaces :: !(DList (Identifier, Attributed (AST.Interface AST.Complete)))
  , namespaces :: !(DList (Identifier, Attributed AST.Namespace))
  , mixins :: !(DList (Identifier, Attributed AST.Mixin))
  , dictionaries :: !(DList (Identifier, Attributed (AST.Dictionary AST.Complete)))
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
  , dictionaries :: !(DList (Identifier, Attributed (AST.Dictionary AST.Partial)))
  , mixins :: !(DList (Identifier, Attributed AST.Mixin))
  }
  deriving (Show, Eq, Ord, Generic)
  deriving (Semigroup, Monoid) via Generically Partials

desugar :: forall t. (Foldable t) => t IDLFragment -> Either DesugarError Definitions
desugar frags = runDesugarer $ do
  let (comps, partials) = classifyFragments frags
  resolveCompletes comps
  resolvePartials partials

classifyFragments :: (Foldable t) => t IDLFragment -> (Completes, Partials)
classifyFragments =
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

resolvePartials :: Partials -> Desugarer ()
resolvePartials = undefined

resolveCompletes :: Completes -> Desugarer ()
resolveCompletes Completes {..} = do
  forM_ mixins \(n, Attributed {..}) -> do
    registerMixin n attributes entry
  forM_ interfaces \(n, Attributed {..}) -> do
    registerInterface n attributes entry
  eith <- uses #inheritance AM.topSort
  case eith of
    Left cyc -> throw $ CyclicInheritance cyc
    Right {} -> pure ()
  pure ()

registerMixin :: Identifier -> V.Vector ExtendedAttribute -> AST.Mixin -> Desugarer ()
registerMixin name atts (AST.Mixin body) = do
  old <-
    #mixins . at name
      <<?= Attributed {entry = desugarMixin body, attributes = atts}
  when (isJust old) $ throw $ MixinAlreadyDefined name

desugarMixin :: V.Vector (Attributed AST.MixinMember) -> Mixin
desugarMixin =
  L.fold do
    constants <- L.handles (attributedL #_MixinConst) dlistL
    operations <- L.handles (attributedL #_MixinOp) dlistL
    stringifiers <- L.handles (attributedL #_MixinStringifier) dlistL
    attributes <- L.handles (attributedL #_MixinAttribute) dlistL
    pure Mixin {..}

attributedL :: Lens.Fold a b -> Lens.Fold (Attributed a) (Attributed b)
attributedL orig = Lens.runFold do
  attributes <- Lens.Fold #attributes
  entry <- Lens.Fold $ #entry . orig
  pure Attributed {..}

dlistL :: L.Fold a (DList a)
dlistL = DL.fromList <$> L.list

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

desugarInterface ::
  forall p.
  (AST.KnownPartiality p) =>
  V.Vector (Attributed (AST.InterfaceMember p)) ->
  Interface
desugarInterface =
  L.fold do
    constructors <- case sPartiality @p of
      SComplete -> L.handles (attributedL AST._Constructor) dlistL
      SPartial -> pure mempty
    constants <- L.handles (attributedL AST._IfConst) dlistL
    operations <- L.handles (attributedL AST._IfOperation) dlistL
    iterables <- L.handles (attributedL AST._IfIterable) dlistL
    asyncIterables <- L.handles (attributedL AST._IfAsyncIterable) dlistL
    stringifiers <- L.handles (attributedL AST._IfStringifier) dlistL
    attributes <- L.handles (attributedL AST._IfAttribute) dlistL
    statics <-
      L.handles (attributedL AST._IfAttribute) do
        atts <- undefined
        ops <- undefined
        pure (atts, ops)
    maplikes <- L.handles (attributedL AST._IfMaplike) dlistL
    setlikes <- L.handles (attributedL AST._IfSetlike) dlistL
    inheritedAttributes <- L.handles (attributedL AST._IfInherit) dlistL
    pure $
      let (staticAttributes, staticOperations) = statics
       in Interface {..}
