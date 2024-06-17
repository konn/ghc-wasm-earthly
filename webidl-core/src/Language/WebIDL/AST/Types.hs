{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NoFieldSelectors #-}

module Language.WebIDL.AST.Types (
  IDLFragment (..),
  Attributed (..),
  Inheritance (..),
  ExtendedAttribute (..),
  Namespace (..),
  NamespaceMember (..),
  ArgNameKeyword (..),
  Attribute (..),
  AttributeName (..),
  Dictionary (..),
  StaticMember (..),
  DictionaryMember (..),
  Definition (..),
  Maplike (..),
  Setlike (..),
  Iterable (..),
  AsyncIterable (..),
  Access (..),
  Interface (..),
  InterfaceMember (..),
  _Constructor,
  _IfConst,
  _IfOperation,
  _IfStringifier,
  _IfStaticMember,
  _IfIterable,
  _IfAsyncIterable,
  _IfAttribute,
  _IfMaplike,
  _IfSetlike,
  _IfInherit,
  CallbackInterface (..),
  CallbackInterfaceMember (..),
  Mixin (..),
  MixinMember (..),
  Enum_ (..),
  Typedef (..),
  IncludesStatement (..),
  CallbackFunction (..),
  IDLType (..),
  UnionType (..),
  DistinguishableType (..),
  WithNullarity (..),
  StringType (..),
  BufferType (..),
  Identifier,
  Operation (..),
  Special (..),
  RegularOperation (..),
  OperationName (..),
  ArgumentList (..),
  Argument (..),
  OptionalArgument (..),
  ArgumentName (..),
  Ellipsis (..),
  Const (..),
  ConstType (..),
  ConstValue (..),
  Restriction (..),
  Sign (..),
  PrimType (..),
  DefaultValue (..),
  Stringifier (..),
  Partiality (..),
  Complete,
  Partial,
  SPartiality (..),
  KnownPartiality (..),
) where

import Control.Lens (Prism, Prism', prism)
import Data.Data (Data)
import Data.List.NonEmpty (NonEmpty)
import Data.Scientific (Scientific)
import Data.Text qualified as T
import Data.Type.Equality (TestEquality (..), (:~:) (..))
import Data.Vector qualified as V
import GHC.Generics

-- | NOTE: WebIDL spec is too ambiguous - @ExtendedAttribute@ nonterminal symbol seems unmatching with any of ExtendedAttributeNoArgs, etc.
data ExtendedAttribute
  = ExtendedAttributeNoArgs !Identifier
  | ExtendedAttributeArgList !Identifier !ArgumentList
  | ExtendedAttributeIdent !Identifier !Identifier
  | ExtendedAttributeWildcard !Identifier
  | ExtendedAttributeIdentList !Identifier !(V.Vector Identifier)
  | ExtendedAttributeNamedArgList !Identifier !Identifier !ArgumentList
  deriving (Data, Show, Eq, Ord, Generic)

data Attributed a = Attributed
  { attributes :: !(V.Vector ExtendedAttribute)
  , entry :: !a
  }
  deriving (Data, Show, Eq, Ord, Generic, Generic1, Functor, Foldable, Traversable)

instance (Semigroup a) => Semigroup (Attributed a) where
  Attributed a1 x1 <> Attributed a2 x2 = Attributed (a1 <> a2) (x1 <> x2)

instance (Monoid a) => Monoid (Attributed a) where
  mempty = Attributed mempty mempty

newtype IDLFragment = IDLFragment [Attributed Definition]
  deriving (Show, Eq, Ord, Generic)

data Inheritance p where
  NoInheritance :: Inheritance p
  Inherits :: Identifier -> Inheritance Complete

deriving instance Data (Inheritance Complete)

deriving instance Show (Inheritance p)

deriving instance Eq (Inheritance p)

deriving instance Ord (Inheritance p)

data Interface p = Interface !(Inheritance p) !(V.Vector (Attributed (InterfaceMember p)))
  deriving (Show, Eq, Ord, Generic)

data Special = Legacycaller | Getter | Setter | Deleter
  deriving (Data, Show, Eq, Ord, Generic)

data Operation
  = Operation !(Maybe Special) !RegularOperation
  deriving (Data, Show, Eq, Ord, Generic)

data StaticMember
  = StaticAttribute !Access !Attribute
  | StaticOp !RegularOperation
  deriving (Data, Show, Eq, Ord, Generic)

data Iterable = Iterable !(Attributed IDLType) !(Maybe (Attributed IDLType))
  deriving (Data, Show, Eq, Ord, Generic)

data AsyncIterable
  = AsyncIterable
      !(Attributed IDLType)
      !(Maybe (Attributed IDLType))
      !(Maybe ArgumentList)
  deriving (Data, Show, Eq, Ord, Generic)

data Maplike = Maplike !(Attributed IDLType) !(Attributed IDLType)
  deriving (Data, Show, Eq, Ord, Generic)

newtype Setlike = Setlike (Attributed IDLType)
  deriving (Data, Show, Eq, Ord, Generic)

data Access = ReadOnly | ReadWrite
  deriving (Data, Show, Eq, Ord, Generic, Enum, Bounded)

data InterfaceMember p where
  Constructor :: ArgumentList -> InterfaceMember Complete
  IfConst :: Const -> InterfaceMember p
  IfOperation :: Operation -> InterfaceMember p
  IfStringifier :: Stringifier -> InterfaceMember p
  IfStaticMember :: StaticMember -> InterfaceMember p
  IfIterable :: Iterable -> InterfaceMember p
  IfAsyncIterable :: AsyncIterable -> InterfaceMember p
  IfAttribute :: Access -> Attribute -> InterfaceMember p
  IfMaplike :: Access -> Maplike -> InterfaceMember p
  IfSetlike :: Access -> Setlike -> InterfaceMember p
  IfInherit :: Attribute -> InterfaceMember p

_Constructor :: Prism (InterfaceMember p) (InterfaceMember Complete) ArgumentList ArgumentList
_Constructor = prism Constructor \case
  Constructor x -> Right x
  IfConst x -> Left (IfConst x)
  IfOperation x -> Left (IfOperation x)
  IfStringifier x -> Left (IfStringifier x)
  IfStaticMember x -> Left (IfStaticMember x)
  IfIterable x -> Left (IfIterable x)
  IfAsyncIterable x -> Left (IfAsyncIterable x)
  IfAttribute x y -> Left (IfAttribute x y)
  IfMaplike x y -> Left (IfMaplike x y)
  IfSetlike rw x -> Left (IfSetlike rw x)
  IfInherit x -> Left (IfInherit x)

_IfConst :: Prism' (InterfaceMember p) Const
_IfConst = prism IfConst \case
  IfConst x -> Right x
  x -> Left x

_IfOperation :: Prism' (InterfaceMember p) Operation
_IfOperation = prism IfOperation \case
  IfOperation x -> Right x
  x -> Left x

_IfStringifier :: Prism' (InterfaceMember p) Stringifier
_IfStringifier = prism IfStringifier \case
  IfStringifier x -> Right x
  x -> Left x

_IfStaticMember :: Prism' (InterfaceMember p) StaticMember
_IfStaticMember = prism IfStaticMember \case
  IfStaticMember x -> Right x
  x -> Left x

_IfIterable :: Prism' (InterfaceMember p) Iterable
_IfIterable = prism IfIterable \case
  IfIterable x -> Right x
  x -> Left x

_IfAsyncIterable :: Prism' (InterfaceMember p) AsyncIterable
_IfAsyncIterable = prism IfAsyncIterable \case
  IfAsyncIterable x -> Right x
  x -> Left x

_IfAttribute :: Prism' (InterfaceMember p) (Access, Attribute)
_IfAttribute = prism (uncurry IfAttribute) \case
  IfAttribute x y -> Right (x, y)
  x -> Left x

_IfMaplike :: Prism' (InterfaceMember p) (Access, Maplike)
_IfMaplike = prism (uncurry IfMaplike) \case
  IfMaplike x y -> Right (x, y)
  x -> Left x

_IfSetlike :: Prism' (InterfaceMember p) (Access, Setlike)
_IfSetlike = prism (uncurry IfSetlike) \case
  IfSetlike x y -> Right (x, y)
  x -> Left x

_IfInherit :: Prism' (InterfaceMember p) Attribute
_IfInherit = prism IfInherit \case
  IfInherit x -> Right x
  x -> Left x

deriving instance Show (InterfaceMember p)

deriving instance Eq (InterfaceMember p)

deriving instance Ord (InterfaceMember p)

newtype Mixin = Mixin (V.Vector (Attributed MixinMember))
  deriving (Data, Show, Eq, Ord, Generic)

data Stringifier
  = Stringifier
  | AttributeStringifier !Access !Attribute
  | -- | It doesn't appear in WebIDL standard, but there is...
    OperationStringifier !RegularOperation
  deriving (Data, Show, Eq, Ord, Generic)

data MixinMember
  = MixinConst !Const
  | MixinOp !RegularOperation
  | MixinStringifier !Stringifier
  | MixinAttribute
      !Access
      !Attribute
  deriving (Data, Show, Eq, Ord, Generic)

data CallbackInterface
  = CallbackInterface
  { callbackOperation :: RegularOperation
  , callbackConsts :: V.Vector (Attributed Const)
  }
  deriving (Data, Show, Eq, Ord, Generic)

data CallbackInterfaceMember
  = CallbackInterfaceConst Const
  | CallbackInterfaceRegularOperation RegularOperation
  deriving (Data, Show, Eq, Ord, Generic)

data RegularOperation = RegularOperation
  { returnType :: !IDLType
  , operationName :: !(Maybe OperationName)
  , args :: !ArgumentList
  }
  deriving (Data, Show, Eq, Ord, Generic)

data OperationName = IncludesOperation | OperationNamed !Identifier
  deriving (Data, Show, Eq, Ord, Generic)

data Const = Const !ConstType !Identifier !ConstValue
  deriving (Data, Show, Eq, Ord, Generic)

data ConstType = PrimConstType PrimType | IdentConstType Identifier
  deriving (Data, Show, Eq, Ord, Generic)

data Restriction = Unrestricted | Restricted
  deriving (Data, Show, Eq, Ord, Generic)

data Sign = Signed | Unsigned
  deriving (Data, Show, Eq, Ord, Generic, Enum, Bounded)

data PrimType = Short Sign | Long Sign | LongLong Sign | Float Restriction | Double Restriction | Boolean | Byte | Octet | Bigint
  deriving (Data, Show, Eq, Ord, Generic)

data ConstValue = Bool Bool | Decimal Scientific | Infinity | MinusInfinity | NaN | Integer Integer
  deriving (Data, Show, Eq, Ord, Generic)

data Partiality = Partial | Complete
  deriving (Data, Show, Eq, Ord, Generic)

data SPartiality p where
  SPartial :: SPartiality 'Partial
  SComplete :: SPartiality 'Complete

deriving instance Show (SPartiality p)

deriving instance Eq (SPartiality p)

deriving instance Ord (SPartiality p)

class KnownPartiality p where
  sPartiality :: SPartiality p

instance KnownPartiality 'Partial where
  sPartiality = SPartial

instance KnownPartiality 'Complete where
  sPartiality = SComplete

instance TestEquality SPartiality where
  testEquality = \cases
    SPartial SPartial -> Just Refl
    SComplete SComplete -> Just Refl
    _ _ -> Nothing

type Partial = 'Partial

type Complete = 'Complete

data Definition
  = InterfaceD !Identifier !(Interface Complete)
  | PartialInterfaceD !Identifier (Interface Partial)
  | InterfaceMixinD !Identifier !Mixin
  | PartialMixinD !Identifier !Mixin
  | CallbackFunctionD !Identifier !CallbackFunction
  | CallbackInterfaceD !Identifier !CallbackInterface
  | NamespaceD !Identifier !Namespace
  | PartialNamespaceD !Identifier !Namespace
  | DictionaryD !Identifier !(Dictionary Complete)
  | PartialDictionaryD !Identifier !(Dictionary Partial)
  | TypedefD !Identifier !Typedef
  | EnumD !Identifier !Enum_
  | IncludesStatementD !Identifier IncludesStatement
  deriving (Show, Eq, Ord, Generic)

data CallbackFunction = CallbackFunction {returnType :: IDLType, argTypes :: ArgumentList}
  deriving (Data, Show, Eq, Ord, Generic)

data WithNullarity a
  = Plain a
  | Nullable a
  deriving (Data, Show, Eq, Ord, Generic, Functor, Foldable, Traversable)

data StringType = ByteString | DOMString | USVString
  deriving (Data, Show, Eq, Ord, Generic)

data BufferType
  = ArrayBuffer
  | SharedArrayBuffer
  | DataView
  | Int8Array
  | Int16Array
  | Int32Array
  | Uint8Array
  | Uint16Array
  | Uint32Array
  | Uint8ClampedArray
  | BigInt64Array
  | BigUint64Array
  | Float32Array
  | Float64Array
  deriving (Data, Show, Eq, Ord, Generic, Enum, Bounded)

data DistinguishableType
  = DPrim !PrimType
  | DString !StringType
  | DNamed !Identifier
  | DSequence !(Attributed IDLType)
  | DObject
  | DSymbol
  | DBuffer !BufferType
  | DFrozenArray !(Attributed IDLType)
  | DObservableArray !(Attributed IDLType)
  | DRecord !StringType !(Attributed IDLType)
  | DUndefined
  deriving (Data, Show, Eq, Ord, Generic)

data IDLType
  = Distinguishable !(WithNullarity DistinguishableType)
  | AnyType
  | PromiseType !IDLType
  | UnionType !(WithNullarity UnionType)
  deriving (Data, Show, Eq, Ord, Generic)

newtype UnionType = MkUnionType (NonEmpty (WithNullarity (Either (Attributed DistinguishableType) UnionType)))
  deriving (Data, Show, Eq, Ord, Generic)

data ArgumentList = ArgumentList
  { requiredArgs :: !(V.Vector (Attributed Argument))
  , optionalArgs :: !(V.Vector (Attributed OptionalArgument))
  , ellipsis :: !(Maybe (Attributed Ellipsis))
  }
  deriving (Data, Show, Eq, Ord, Generic)

data OptionalArgument
  = OptionalArgument !(Attributed IDLType) !ArgumentName !(Maybe DefaultValue)
  deriving (Data, Show, Eq, Ord, Generic)

data Argument = Argument !IDLType !ArgumentName
  deriving (Data, Show, Eq, Ord, Generic)

data Ellipsis = Ellipsis !IDLType !ArgumentName
  deriving (Data, Show, Eq, Ord, Generic)

data ArgNameKeyword
  = ArgAsync
  | ArgAttribute
  | ArgCallback
  | ArgConst
  | ArgConstructor
  | ArgDeleter
  | ArgDictionary
  | ArgEnum
  | ArgGetter
  | ArgIncludes
  | ArgInherit
  | ArgInterface
  | ArgIterable
  | ArgMaplike
  | ArgMixin
  | ArgNamespace
  | ArgPartial
  | ArgReadonly
  | ArgRequired
  | ArgSetlike
  | ArgSetter
  | ArgStatic
  | ArgStringifier
  | ArgTypedef
  | ArgUnrestricted
  deriving (Data, Show, Eq, Ord, Generic, Enum, Bounded)

data ArgumentName
  = ArgKeyword ArgNameKeyword
  | ArgIdent !Identifier
  deriving (Data, Show, Eq, Ord, Generic)

newtype Namespace = Namespace (V.Vector (Attributed NamespaceMember))
  deriving (Data, Show, Eq, Ord, Generic)

data NamespaceMember
  = NamespaceOp !RegularOperation
  | NamespaceReadOnly !Attribute
  | NamespaceConst !Const
  deriving (Data, Show, Eq, Ord, Generic)

data Attribute = Attribute !(Attributed IDLType) !AttributeName
  deriving (Data, Show, Eq, Ord, Generic)

data AttributeName = AsyncAttribute | RequiredAttribute | AttributeName !Identifier
  deriving (Data, Show, Eq, Ord, Generic)

data Dictionary p = Dictionary !(Inheritance p) !(V.Vector (Attributed DictionaryMember))
  deriving (Show, Eq, Ord, Generic)

data DictionaryMember
  = RequiredMember !IDLType !Identifier
  | OptionalMember !IDLType !Identifier !(Maybe DefaultValue)
  deriving (Data, Show, Eq, Ord, Generic)

data DefaultValue
  = DefaultConst !ConstValue
  | DefaultString !T.Text
  | DefaultEmptyArray
  | DefaultEmptyObject
  | DefaultNull
  | DefaultUndefined
  deriving (Data, Show, Eq, Ord, Generic)

newtype Enum_ = Enum_ (NonEmpty T.Text)
  deriving (Data, Show, Eq, Ord, Generic)

newtype Typedef = Typedef (Attributed IDLType)
  deriving (Data, Show, Eq, Ord, Generic)

newtype IncludesStatement = IncludesStatement Identifier
  deriving (Data, Show, Eq, Ord, Generic)

type Identifier = T.Text
