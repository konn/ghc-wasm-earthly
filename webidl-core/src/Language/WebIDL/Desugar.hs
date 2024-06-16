{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoFieldSelectors #-}

module Language.WebIDL.Desugar (desugar, module Language.WebIDL.Desugar.Types) where

import Algebra.Graph.AdjacencyMap qualified as AM
import Algebra.Graph.AdjacencyMap.Algorithm qualified as AM
import Control.Foldl qualified as L
import Control.Lens (
  At (at),
  foldMapOf,
  folded,
  ix,
  use,
  uses,
  (&),
  (.~),
  (<<?=),
  (<>=),
  (?=),
 )
import Control.Lens qualified as Lens
import Control.Monad (forM_, when)
import Data.DList (DList)
import Data.DList qualified as DL
import Data.Generics.Labels ()
import Data.Maybe (isJust)
import Data.Monoid (First (..))
import Data.Vector qualified as V
import GHC.Generics (Generic, Generically (..))
import Language.WebIDL.AST.Types (IDLFragment, Inheritance (..), SPartiality (..), sPartiality)
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
resolvePartials Partials {..} = do
  forM_ mixins \(n, Attributed {entry = AST.Mixin body, ..}) -> do
    #mixins . ix n <>= Attributed {entry = desugarMixin body, ..}
  forM_ interfaces \(n, Attributed {entry = AST.Interface ih body, ..}) -> do
    let conss = extractLegacyConstructors attributes
    #interfaces . ix n <>= Attributed {entry = desugarInterface ih body <> mempty {constructors = L.fold dlistL conss}, ..}
  forM_ inclusions \(n, AST.IncludesStatement mixin) -> do
    ma <- use $ #mixins . at mixin
    case ma of
      Nothing -> throw $ MixinNotFound mixin
      Just mems ->
        #interfaces . ix n . #entry <>= mixinToInterface mems.entry
  forM_ namespaces \(name, Attributed {entry = AST.Namespace body, ..}) ->
    #namespaces . at name
      <<?= Attributed
        { entry = desugarNamespace body
        , attributes
        }
  forM_ dictionaries \(name, Attributed {entry = AST.Dictionary NoInheritance body}) ->
    #dictionaries . at name . Lens._Just . #entry <>= desugarDictionary body

resolveCompletes :: Completes -> Desugarer ()
resolveCompletes Completes {..} = do
  forM_ callbackFunctions \(n, Attributed {..}) -> do
    #callbackFunctions . at n ?= Attributed {entry = entry, ..}
  forM_ callbackInterfaces \(n, Attributed {..}) -> do
    #callbackInterfaces . at n ?= Attributed {entry = entry, ..}
  forM_ enumerations \(n, Attributed {entry = AST.Enum_ body, ..}) -> do
    #enums . at n ?= Attributed {entry = Enumeration body, ..}
  forM_ typedefs \(n, Attributed {entry = AST.Typedef body, ..}) -> do
    #typedefs . at n ?= Attributed {entry = body, ..}
  forM_ interfaces \(n, Attributed {..}) -> do
    registerInterface n attributes entry
  eith <- uses #interfaceInheritance AM.topSort
  forM_ mixins \(n, Attributed {..}) -> do
    registerMixin n attributes entry
  forM_ namespaces \(n, Attributed {..}) -> do
    registerNamespace n attributes entry
  case eith of
    Left cyc -> throw $ CyclicInheritance cyc
    Right {} -> pure ()
  forM_ dictionaries \(n, Attributed {..}) -> do
    registerDictionary n attributes entry
  dictGraph <- use #dictionaryInheritance
  case AM.topSort dictGraph of
    Left cyc -> throw $ CyclicInheritance cyc
    Right tieBreak -> do
      forM_ tieBreak \targ -> do
        forM_ (AM.postSet targ dictGraph) \p -> do
          use (#dictionaries . at p) >>= mapM_ \parent -> do
            #dictionaries . at targ . Lens._Just . #entry <>= parent.entry

registerDictionary :: Identifier -> V.Vector ExtendedAttribute -> AST.Dictionary AST.Complete -> Desugarer ()
registerDictionary name atts (AST.Dictionary inh body) = do
  old <-
    #dictionaries . at name
      <<?= Attributed
        { entry = desugarDictionary body
        , attributes = atts
        }
  when (isJust old) $ throw $ DictionaryAlreadyDefined name
  case inh of
    AST.NoInheritance -> pure ()
    AST.Inherits super ->
      #dictionaryInheritance <>= AM.edge name super

desugarDictionary :: V.Vector (Attributed AST.DictionaryMember) -> Dictionary
desugarDictionary = L.fold do
  requiredMembers <-
    L.handles (attributedL #_RequiredMember) $
      L.premap
        ( \Attributed {entry = (typ, name), ..} ->
            (name, Attributed {entry = typ, ..})
        )
        L.map
  optionalMembers <-
    L.handles (attributedL #_OptionalMember) $
      L.premap
        ( \Attributed {entry = (typ, name, def), ..} ->
            (name, Attributed {entry = (typ, def), ..})
        )
        L.map
  pure Dictionary {..}

registerNamespace :: Identifier -> V.Vector ExtendedAttribute -> AST.Namespace -> Desugarer ()
registerNamespace name atts (AST.Namespace body) = do
  old <-
    #namespaces . at name
      <<?= Attributed
        { entry = desugarNamespace body
        , attributes = atts
        }
  when (isJust old) $ throw $ MixinAlreadyDefined name

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

desugarNamespace :: V.Vector (Attributed AST.NamespaceMember) -> Namespace' DList
desugarNamespace =
  L.fold do
    constants <- L.handles (attributedL #_NamespaceConst) dlistL
    operations <- L.handles (attributedL #_NamespaceOp) dlistL
    readOnlyAttributes <- L.handles (attributedL #_NamespaceReadOnly) dlistL
    pure Namespace {..}

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
  let conss = extractLegacyConstructors atts
  old <-
    #interfaces . at name
      <<?= Attributed {entry = desugarInterface inh body <> mempty {constructors = DL.fromList (V.toList conss)}, attributes = atts}
  when (isJust old) $ throw $ InterfaceAlreadyDefined name
  case inh of
    AST.NoInheritance -> pure ()
    AST.Inherits super ->
      #interfaceInheritance <>= AM.edge name super

extractLegacyConstructors :: V.Vector ExtendedAttribute -> V.Vector (Attributed ArgumentList)
extractLegacyConstructors = V.mapMaybe \case
  AST.ExtendedAttributeArgList "Constructor" args ->
    Just $ Attributed {entry = args, attributes = mempty}
  _ -> Nothing

desugarInterface ::
  forall p.
  (AST.KnownPartiality p) =>
  Inheritance p ->
  V.Vector (Attributed (AST.InterfaceMember p)) ->
  Interface' DList
desugarInterface i =
  let parent = case i of
        NoInheritance -> First Nothing
        Inherits p -> First $ Just p
   in L.fold do
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
          L.handles (attributedL AST._IfStaticMember) do
            atts <- L.handles (attributedL #_StaticAttribute) dlistL
            ops <- L.handles (attributedL #_StaticOp) dlistL
            pure (atts, ops)
        maplikes <- L.handles (attributedL AST._IfMaplike) dlistL
        setlikes <- L.handles (attributedL AST._IfSetlike) dlistL
        inheritedAttributes <- L.handles (attributedL AST._IfInherit) dlistL
        pure $
          let (staticAttributes, staticOperations) = statics
           in Interface {..}
