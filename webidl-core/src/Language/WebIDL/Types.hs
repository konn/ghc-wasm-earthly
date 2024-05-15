{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NoFieldSelectors #-}

module Language.WebIDL.Types (
  IDLFragment (..),
  Attributed (..),
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
  ArgumentName (..),
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
  deriving (Show, Eq, Ord, Generic)

data Attributed a = Attributed
  { attributes :: !(V.Vector ExtendedAttribute)
  , entry :: !a
  }
  deriving (Show, Eq, Ord, Generic)

newtype IDLFragment = IDLFragment [Attributed Definition]
  deriving (Show, Eq, Ord, Generic)

data Interface p = Interface !(Maybe Identifier) !(V.Vector (Attributed (InterfaceMember p)))
  deriving (Show, Eq, Ord, Generic)

data Special = Getter | Setter | Deleter
  deriving (Show, Eq, Ord, Generic)

data Operation
  = Operation !(Maybe Special) !RegularOperation
  deriving (Show, Eq, Ord, Generic)

data StaticMember
  = StaticAttribute !Access !Attribute
  | StaticOp !RegularOperation
  deriving (Show, Eq, Ord, Generic)

data Iterable = Iterable !(Attributed IDLType) !(Maybe (Attributed IDLType))
  deriving (Show, Eq, Ord, Generic)

data AsyncIterable
  = AsyncIterable
      !(Attributed IDLType)
      !(Maybe (Attributed IDLType))
      !(Maybe ArgumentList)
  deriving (Show, Eq, Ord, Generic)

data Maplike = Maplike !(Attributed IDLType) !(Attributed IDLType)
  deriving (Show, Eq, Ord, Generic)

newtype Setlike = Setlike (Attributed IDLType)
  deriving (Show, Eq, Ord, Generic)

data Access = ReadOnly | ReadWrite
  deriving (Show, Eq, Ord, Generic, Enum, Bounded)

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

deriving instance Show (InterfaceMember p)

deriving instance Eq (InterfaceMember p)

deriving instance Ord (InterfaceMember p)

newtype Mixin = Mixin (V.Vector (Attributed MixinMember))
  deriving (Show, Eq, Ord, Generic)

data Stringifier
  = Stringifier
  | AttributeStringifier !Access !Attribute
  deriving (Show, Eq, Ord, Generic)

data MixinMember
  = MixinConst !Const
  | MixinOp !RegularOperation
  | MixinStringifier !Stringifier
  | MixinAttribute
      -- | 'True' if @required@
      !Bool
      !Attribute
  deriving (Show, Eq, Ord, Generic)

newtype CallbackInterface
  = CallbackInterface (V.Vector (Attributed CallbackInterfaceMember))
  deriving (Show, Eq, Ord, Generic)

data CallbackInterfaceMember
  = CallbackInterfaceConst Const
  | CallbackInterfaceRegularOperation RegularOperation
  deriving (Show, Eq, Ord, Generic)

data RegularOperation = RegularOperation !IDLType !(Maybe OperationName) !ArgumentList
  deriving (Show, Eq, Ord, Generic)

data OperationName = IncludesOperation | OperationNamed !Identifier
  deriving (Show, Eq, Ord, Generic)

data Const = Const !ConstType !Identifier !ConstValue
  deriving (Show, Eq, Ord, Generic)

data ConstType = PrimConstType PrimType | IdentConstType Identifier
  deriving (Show, Eq, Ord, Generic)

data Restriction = Unrestricted | Restricted
  deriving (Show, Eq, Ord, Generic)

data Sign = Signed | Unsigned
  deriving (Show, Eq, Ord, Generic, Enum, Bounded)

data PrimType = Short Sign | Long Sign | LongLong Sign | Float Restriction | Double Restriction | Boolean | Byte | Octet | Bigint
  deriving (Show, Eq, Ord, Generic)

data ConstValue = Bool Bool | Decimal Scientific | Infinity | MinusInfinity | NaN | Integer Integer
  deriving (Show, Eq, Ord, Generic)

data Partiality = Partial | Complete
  deriving (Show, Eq, Ord, Generic)

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
  | DictionaryD !Identifier !Dictionary
  | PartialDictionaryD !Identifier !Dictionary
  | TypedefD !Identifier !Typedef
  | EnumD !Identifier !Enum_
  | IncludesStatementD !Identifier IncludesStatement
  deriving (Show, Eq, Ord, Generic)

data CallbackFunction = CallbackFunction IDLType ArgumentList
  deriving (Show, Eq, Ord, Generic)

data WithNullarity a
  = Plain a
  | Nullable a
  deriving (Show, Eq, Ord, Generic, Functor)

data StringType = ByteString | DOMString | USVString
  deriving (Show, Eq, Ord, Generic)

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
  deriving (Show, Eq, Ord, Generic, Enum, Bounded)

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
  deriving (Show, Eq, Ord, Generic)

data IDLType
  = Distinguishable !(WithNullarity DistinguishableType)
  | AnyType
  | PromiseType !IDLType
  | UnionType !(Maybe UnionType)
  deriving (Show, Eq, Ord, Generic)

newtype UnionType = MkUnionType (NonEmpty (Either (Attributed IDLType) (Maybe UnionType)))
  deriving (Show, Eq, Ord, Generic)

newtype ArgumentList = ArgumentList (V.Vector (Attributed Argument))
  deriving (Show, Eq, Ord, Generic)

data Argument
  = OptionalArg !(Attributed IDLType) !ArgumentName !(Maybe DefaultValue)
  | RequiredArg
      !IDLType
      -- | 'True' if ellipsis (@...@) is given
      !Bool
      !ArgumentName
  deriving (Show, Eq, Ord, Generic)

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
  deriving (Show, Eq, Ord, Generic, Enum, Bounded)

data ArgumentName
  = ArgKeyword ArgNameKeyword
  | ArgIdent !Identifier
  deriving (Show, Eq, Ord, Generic)

newtype Namespace = Namespace (V.Vector (Attributed NamespaceMember))
  deriving (Show, Eq, Ord, Generic)

data NamespaceMember
  = NamespaceOp !RegularOperation
  | NamespaceReadOnly !Attribute
  | NamespaceConst !Const
  deriving (Show, Eq, Ord, Generic)

data Attribute = Attribute !(Attributed IDLType) !AttributeName
  deriving (Show, Eq, Ord, Generic)

data AttributeName = AsyncAttribute | RequiredAttribute | AttributeName !Identifier
  deriving (Show, Eq, Ord, Generic)

data Dictionary = Dictionary !(Maybe Identifier) !(V.Vector (Attributed DictionaryMember))
  deriving (Show, Eq, Ord, Generic)

data DictionaryMember
  = RequiredMember !(Attributed IDLType) !Identifier
  | OptionalMember !IDLType !(Maybe DefaultValue)
  deriving (Show, Eq, Ord, Generic)

data DefaultValue
  = DefaultConst !ConstValue
  | DefaultString
  | DefaultEmptyArray
  | DefaultEmptyObject
  | DefaultNull
  | DefaultUndefined
  deriving (Show, Eq, Ord, Generic)

data Enum_ = Enum_ (NonEmpty T.Text)
  deriving (Show, Eq, Ord, Generic)

newtype Typedef = Typedef (Attributed IDLType)
  deriving (Show, Eq, Ord, Generic)

data IncludesStatement = IncludesStatement Identifier
  deriving (Show, Eq, Ord, Generic)

type Identifier = T.Text
