{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Language.WebIDL.AST.Parser (
  parseIDLFragment,
  definitionsP,
  definitionP,
) where

import Control.Applicative hiding (Const)
import Control.Applicative.Combinators.NonEmpty qualified as PNE
import Data.Char qualified as C
import Data.Either (partitionEithers)
import Data.Functor.Identity
import Data.HashSet qualified as HS
import Data.Scientific
import Data.Text qualified as T
import Data.Tuple (swap)
import Data.Vector qualified as V
import Data.Void (Void)
import Language.WebIDL.AST.Types
import Text.Megaparsec ((<?>))
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as L

parseIDLFragment :: T.Text -> Either (P.ParseErrorBundle T.Text Void) IDLFragment
parseIDLFragment = P.runParser (spaceComment *> definitionsP <* P.eof) "<input>"

type Parser = P.ParsecT Void T.Text Identity

spaceComment :: Parser ()
spaceComment = L.space P.space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceComment

symbol :: T.Text -> Parser T.Text
symbol = L.symbol spaceComment

parens :: Parser a -> Parser a
parens = P.between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = P.between (symbol "{") (symbol "}")

angles :: Parser a -> Parser a
angles = P.between (symbol "<") (symbol ">")

brackets :: Parser a -> Parser a
brackets = P.between (symbol "[") (symbol "]")

semi :: Parser T.Text
semi = symbol ";" <?> "semicolon"

colon :: Parser T.Text
colon = symbol ":" <?> "colon"

equals :: Parser T.Text
equals = symbol "="

comma :: Parser T.Text
comma = symbol ","

reserved :: T.Text -> Parser ()
reserved w =
  lexeme $
    P.try
      ( (P.string w *> P.notFollowedBy P.alphaNumChar)
          <?> ("reserved word " <> T.unpack w)
      )

anyIdentifier :: Parser T.Text
anyIdentifier = lexeme $ do
  prfx <- P.option "" $ T.singleton <$> (P.char '_' <|> P.char '-')
  hd <- P.letterChar
  tl <-
    P.takeWhileP
      Nothing
      (\c -> C.isAscii c && (C.isLetter c || C.isDigit c || c == '-' || c == '_'))
  let body = prfx <> (hd `T.cons` tl)
  if body `HS.member` keywords
    then fail $ "reserved word " <> T.unpack body
    else pure body

integerP :: Parser Integer
integerP =
  lexeme $
    L.signed spaceComment $
      ( P.try ("0" *> ((P.string "x" <|> P.string "X") *> L.hexadecimal <|> L.octal))
          <|> P.notFollowedBy (P.char '0') *> P.try L.decimal
      )
        <* P.notFollowedBy (P.char 'e' <|> P.char 'E' <|> P.char '.')

anyString :: Parser T.Text
anyString =
  lexeme $
    P.between
      (P.char '"')
      (P.char '"')
      (P.takeWhileP (Just "non-double-quote") (/= '"'))

decimal :: Parser Scientific
decimal = lexeme $ P.try L.scientific

definitionsP :: Parser IDLFragment
definitionsP = IDLFragment <$> P.many (attributedP definitionP)

attributedP :: Parser a -> Parser (Attributed a)
attributedP p = do
  attributes <- extendedAttributeListP
  entry <- p
  pure Attributed {..}

definitionP :: Parser Definition
definitionP =
  callbackOrInterfaceOrMixinP
    <|> partialP
    <|> uncurry NamespaceD <$> namespaceP
    <|> uncurry DictionaryD <$> dictionaryP
    <|> uncurry EnumD <$> enumP
    <|> uncurry TypedefD <$> typedefD
    <|> uncurry IncludesStatementD <$> includesStatementP

partialP :: Parser Definition
partialP =
  reserved "partial"
    *> ( reserved "interface"
          *> ( uncurry PartialMixinD <$> P.try mixinRestP
                <|> uncurry PartialInterfaceD <$> interfaceRestP
             )
          <|> uncurry PartialNamespaceD <$> namespaceP
          <|> uncurry PartialDictionaryD <$> dictionaryP
       )

includesStatementP :: Parser (T.Text, IncludesStatement)
includesStatementP =
  (,)
    <$> anyIdentifier
    <* reserved "includes"
    <*> (IncludesStatement <$> anyIdentifier <* semi)

typedefD :: Parser (T.Text, Typedef)
typedefD =
  fmap swap . (,)
    <$ reserved "typedef"
    <*> (Typedef <$> attributedP idlTypeP)
    <*> anyIdentifier
    <* semi

enumP :: Parser (T.Text, Enum_)
enumP =
  (,)
    <$ reserved "enum"
    <*> anyIdentifier
    <*> braces (Enum_ <$> anyString `PNE.sepEndBy1` comma)
    <* semi

dictionaryP :: (KnownPartiality p) => Parser (T.Text, Dictionary p)
dictionaryP =
  (,)
    <$ reserved "dictionary"
    <*> anyIdentifier
    <*> ( Dictionary
            <$> inheritanceP
            <*> braces (V.fromList <$> many (attributedP dictionaryMemberP))
        )
    <* semi

dictionaryMemberP :: Parser DictionaryMember
dictionaryMemberP =
  RequiredMember
    <$ reserved "required"
    <*> idlTypeP
    <*> anyIdentifier
    <* semi
    <|> OptionalMember
      <$> idlTypeP
      <*> anyIdentifier
      <*> P.optional (symbol "=" *> defaultValueP)
      <* semi

defaultValueP :: Parser DefaultValue
defaultValueP =
  DefaultConst <$> constValueP
    <|> DefaultString <$> anyString
    <|> DefaultEmptyArray <$ symbol "[" <* symbol "]"
    <|> DefaultEmptyObject <$ symbol "{" <* symbol "}"
    <|> DefaultNull <$ reserved "null"
    <|> DefaultUndefined <$ reserved "undefined"

namespaceP :: Parser (T.Text, Namespace)
namespaceP =
  (,)
    <$ reserved "namespace"
    <*> anyIdentifier
    <*> braces (Namespace . V.fromList <$> many (attributedP namespaceMemberP))
    <* semi

namespaceMemberP :: Parser NamespaceMember
namespaceMemberP =
  NamespaceOp <$> regularOperationP <* semi
    <|> NamespaceReadOnly <$ reserved "readonly" <*> attributeP
    <|> NamespaceConst <$> constP

attributeP :: Parser Attribute
attributeP =
  Attribute
    <$ reserved "attribute"
    <*> attributedP idlTypeP
    <*> attributeNameP
    <* semi

attributeNameP :: Parser AttributeName
attributeNameP =
  ( AsyncAttribute <$ reserved "async"
      <|> RequiredAttribute <$ reserved "required"
      <|> AttributeName <$> anyIdentifier
  )
    <?> "attribute name"

callbackOrInterfaceOrMixinP :: Parser Definition
callbackOrInterfaceOrMixinP = callbackP <|> interfaceOrMixinP

interfaceOrMixinP :: Parser Definition
interfaceOrMixinP =
  symbol "interface"
    *> (uncurry InterfaceMixinD <$> mixinRestP <|> uncurry InterfaceD <$> interfaceRestP)

mixinRestP :: Parser (Identifier, Mixin)
mixinRestP =
  (,)
    <$ reserved "mixin"
    <*> anyIdentifier
    <*> braces (Mixin . V.fromList <$> many (attributedP mixinMemberP))
    <* semi

mixinMemberP :: Parser MixinMember
mixinMemberP =
  MixinConst <$> P.try constP
    <|> MixinOp <$> P.try regularOperationP <* semi
    <|> MixinStringifier <$> P.try stringifierP
    <|> MixinAttribute
      <$> P.option ReadWrite (ReadOnly <$ reserved "readonly")
      <*> attributeP

stringifierP :: Parser Stringifier
stringifierP =
  reserved "stringifier"
    *> ( AttributeStringifier
          <$> P.option ReadWrite (ReadOnly <$ reserved "readonly")
          <*> attributeP
          <|> OperationStringifier <$> P.try regularOperationP <* semi
          <|> Stringifier <$ semi
       )

interfaceRestP :: (KnownPartiality p) => Parser (T.Text, Interface p)
interfaceRestP =
  (,)
    <$> anyIdentifier
    <*> ( Interface
            <$> inheritanceP
            <*> braces (V.fromList <$> many (attributedP interfaceMemberP))
        )
    <* semi

interfaceMemberP :: forall p. (KnownPartiality p) => Parser (InterfaceMember p)
interfaceMemberP =
  case sPartiality @p of
    SComplete -> P.try constructorP <|> bodyP
    SPartial -> bodyP
  where
    constructorP =
      Constructor
        <$ reserved "constructor"
        <*> parens argumentListP
        <* semi
    bodyP :: Parser (InterfaceMember p)
    bodyP =
      IfConst <$> (constP <?> "const member")
        <|> IfOperation <$> P.try (operationP <* semi <?> "operation member")
        <|> IfStringifier <$> P.try (stringifierP <?> "stringifier member")
        <|> IfStaticMember <$> P.try (staticMemberP <?> "static member")
        <|> IfIterable <$> P.try (iterableP <?> "iterable member")
        <|> IfAsyncIterable <$> P.try (asyncIterableP <?> "async iterable member")
        <|> P.option ReadWrite (ReadOnly <$ reserved "readonly") <**> P.try (memberP <?> "generic member")
        <|> IfInherit <$ reserved "inherit" <*> attributeP

memberP :: Parser (Access -> InterfaceMember p)
memberP =
  flip IfMaplike <$> maplikeP
    <|> flip IfSetlike <$> setlikeP
    <|> flip IfAttribute <$> attributeP

maplikeP :: Parser Maplike
maplikeP =
  reserved "maplike"
    *> angles (Maplike <$> attributedP idlTypeP <* comma <*> attributedP idlTypeP)
    <* semi

setlikeP :: Parser Setlike
setlikeP =
  Setlike
    <$ reserved "setlike"
    <*> angles (attributedP idlTypeP)
    <* semi

asyncIterableP :: Parser AsyncIterable
asyncIterableP =
  reserved "async"
    *> reserved "iterable"
    *> angles
      ( AsyncIterable
          <$> attributedP idlTypeP
          <*> P.optional (comma *> attributedP idlTypeP)
      )
    <*> P.optional (parens argumentListP)
    <* semi

iterableP :: Parser Iterable
iterableP =
  reserved "iterable"
    *> angles
      ( Iterable
          <$> attributedP idlTypeP
          <*> P.optional (comma *> attributedP idlTypeP)
      )
    <* semi

staticMemberP :: Parser StaticMember
staticMemberP =
  reserved "static" *> do
    StaticAttribute
      <$> P.option ReadWrite (ReadOnly <$ reserved "readonly")
      <*> P.try attributeP
      <|> StaticOp <$> regularOperationP <* semi

operationP :: Parser Operation
operationP =
  Operation <$> P.optional specialP <*> regularOperationP

specialP :: Parser Special
specialP =
  Getter <$ reserved "getter"
    <|> Setter <$ reserved "setter"
    <|> Deleter <$ reserved "deleter"
    <|> Legacycaller <$ reserved "legacycaller"

callbackP :: Parser Definition
callbackP = reserved "callback" *> callbackRestOrInterfaceP

callbackRestOrInterfaceP :: Parser Definition
callbackRestOrInterfaceP =
  CallbackInterfaceD
    <$ reserved "interface"
    <*> anyIdentifier
    <*> braces callbackInterfaceP
    <* semi
    <|> uncurry CallbackFunctionD <$> callbackRestP

callbackRestP :: Parser (T.Text, CallbackFunction)
callbackRestP =
  (,)
    <$> anyIdentifier
    <* symbol "="
    <*> (CallbackFunction <$> idlTypeP <*> parens argumentListP <* semi)

callbackInterfaceP :: Parser CallbackInterface
callbackInterfaceP = P.try $ do
  (ifs, V.fromList -> callbackConsts) <-
    partitionEithers
      . map
        ( traverse
            \case
              CallbackInterfaceConst c -> Right c
              CallbackInterfaceRegularOperation o -> Left o
        )
      <$> many (attributedP callbackInterfaceMemberP)
  case ifs of
    [fun] -> pure CallbackInterface {callbackOperation = fun, ..}
    _ -> fail "Callback interface must have exactly one operation member."

callbackInterfaceMemberP :: Parser CallbackInterfaceMember
callbackInterfaceMemberP =
  CallbackInterfaceConst <$> P.try constP
    <|> CallbackInterfaceRegularOperation <$> regularOperationP <* semi

regularOperationP :: Parser RegularOperation
regularOperationP =
  RegularOperation
    <$> idlTypeP
    <*> P.optional operationNameP
    <*> parens argumentListP

operationNameP :: Parser OperationName
operationNameP = IncludesOperation <$ reserved "includes" <|> OperationNamed <$> anyIdentifier

constP :: Parser Const
constP =
  Const
    <$ reserved "const"
    <*> constTypeP
    <*> anyIdentifier
    <* equals
    <*> constValueP
    <* semi

constValueP :: Parser ConstValue
constValueP =
  Bool <$> boolLit
    <|> Integer <$> P.try integerP
    <|> floatP

floatP :: Parser ConstValue
floatP = P.try infinityP <|> NaN <$ reserved "NaN" <|> Decimal <$> P.try decimal

infinityP :: Parser ConstValue
infinityP =
  MinusInfinity <$ symbol "-" <* reserved "Infinity"
    <|> Infinity <$ reserved "Infinity"

boolLit :: Parser Bool
boolLit = True <$ reserved "true" <|> False <$ reserved "false"

constTypeP :: Parser ConstType
constTypeP = PrimConstType <$> primTypeP <|> IdentConstType <$> anyIdentifier

primTypeP :: Parser PrimType
primTypeP =
  integerTypeP
    <|> floatTypeP
    <|> Boolean <$ reserved "boolean"
    <|> Byte <$ reserved "byte"
    <|> Octet <$ reserved "octet"
    <|> Bigint <$ reserved "bigint"

floatTypeP :: Parser PrimType
floatTypeP =
  P.option Restricted (Unrestricted <$ reserved "unrestricted")
    <**> ( Float <$ reserved "float"
            <|> Double <$ reserved "double"
         )

integerTypeP :: Parser PrimType
integerTypeP =
  P.option Signed (Unsigned <$ reserved "unsigned")
    <**> ( Short <$ reserved "short"
            <|> reserved "long"
              *> P.option Long (LongLong <$ reserved "long")
         )

inheritanceP ::
  forall p.
  (KnownPartiality p) =>
  Parser (Inheritance p)
inheritanceP =
  case sPartiality @p of
    SComplete ->
      P.option
        NoInheritance
        (Inherits <$ P.try colon <*> anyIdentifier)
        <?> "inheritance"
    SPartial -> NoInheritance <$ P.notFollowedBy (colon *> anyIdentifier)

argumentListP :: Parser ArgumentList
argumentListP = do
  requiredArgs <-
    V.fromList
      <$> P.try (attributedP argumentP) `P.sepEndBy` comma
  optionalArgs <-
    V.fromList
      <$> P.try (attributedP optionalArgumentP) `P.sepEndBy` comma
  ellipsis <- P.optional $ attributedP ellipsisP
  pure ArgumentList {..}

optionalArgumentP :: Parser OptionalArgument
optionalArgumentP =
  OptionalArgument
    <$ reserved "optional"
    <*> attributedP idlTypeP
    <*> argNameP
    <*> P.optional (symbol "=" *> defaultValueP)

argumentP :: Parser Argument
argumentP =
  Argument
    <$ P.notFollowedBy (reserved "optional")
    <*> idlTypeP
    <* P.notFollowedBy (symbol "...")
    <*> argNameP

ellipsisP :: Parser Ellipsis
ellipsisP =
  Ellipsis
    <$ P.notFollowedBy (reserved "optional")
    <*> idlTypeP
    <* symbol "..."
    <*> argNameP

argNameP :: Parser ArgumentName
argNameP =
  ArgKeyword <$> argKeywordP
    <|> ArgIdent <$> anyIdentifier

argKeywordP :: Parser ArgNameKeyword
argKeywordP =
  ArgAsync <$ reserved "async"
    <|> ArgAttribute <$ reserved "attribute"
    <|> ArgCallback <$ reserved "callback"
    <|> ArgConst <$ reserved "const"
    <|> ArgConstructor <$ reserved "constructor"
    <|> ArgDeleter <$ reserved "deleter"
    <|> ArgDictionary <$ reserved "dictionary"
    <|> ArgEnum <$ reserved "enum"
    <|> ArgGetter <$ reserved "getter"
    <|> ArgIncludes <$ reserved "includes"
    <|> ArgInherit <$ reserved "inherit"
    <|> ArgInterface <$ reserved "interface"
    <|> ArgIterable <$ reserved "iterable"
    <|> ArgMaplike <$ reserved "maplike"
    <|> ArgMixin <$ reserved "mixin"
    <|> ArgNamespace <$ reserved "namespace"
    <|> ArgPartial <$ reserved "partial"
    <|> ArgReadonly <$ reserved "readonly"
    <|> ArgRequired <$ reserved "required"
    <|> ArgSetlike <$ reserved "setlike"
    <|> ArgSetter <$ reserved "setter"
    <|> ArgStatic <$ reserved "static"
    <|> ArgStringifier <$ reserved "stringifier"
    <|> ArgTypedef <$ reserved "typedef"
    <|> ArgUnrestricted <$ reserved "unrestricted"

optionally :: Parser a -> Parser (WithNullarity a)
optionally p =
  p <**> P.option Plain (Nullable <$ symbol "?")

idlTypeP :: Parser IDLType
idlTypeP =
  Distinguishable <$> optionally distTypeP
    <|> AnyType <$ reserved "any"
    <|> PromiseType <$ reserved "Promise" <*> angles idlTypeP
    <|> UnionType <$> optionally unionTypeP

unionTypeP :: Parser UnionType
unionTypeP =
  parens $
    MkUnionType
      <$> ( optionally
              ( Left <$> attributedP distTypeP
                  <|> Right <$> unionTypeP
              )
              `PNE.sepBy1` symbol "or"
          )

distTypeP :: Parser DistinguishableType
distTypeP =
  DPrim <$> primTypeP
    <|> DString <$> stringTypeP
    <|> DNamed <$> P.try anyIdentifier
    <|> DSequence <$ reserved "sequence" <*> angles (attributedP idlTypeP)
    <|> DObject <$ reserved "object"
    <|> DSymbol <$ reserved "symbol"
    <|> DBuffer <$> bufferTypeP
    <|> DFrozenArray
      <$ reserved "FrozenArray"
      <*> angles (attributedP idlTypeP)
    <|> DObservableArray
      <$ reserved "FrozenArray"
      <*> angles (attributedP idlTypeP)
    <|> reserved "record"
      *> angles
        ( DRecord
            <$> stringTypeP
            <* comma
            <*> attributedP idlTypeP
        )
    <|> DUndefined <$ reserved "undefined"

bufferTypeP :: Parser BufferType
bufferTypeP =
  ArrayBuffer <$ reserved "ArrayBuffer"
    <|> SharedArrayBuffer <$ reserved "SharedArrayBuffer"
    <|> DataView <$ reserved "DataView"
    <|> Int8Array <$ reserved "Int8Array"
    <|> Int16Array <$ reserved "Int16Array"
    <|> Int32Array <$ reserved "Int32Array"
    <|> Uint8Array <$ reserved "Uint8Array"
    <|> Uint16Array <$ reserved "Uint16Array"
    <|> Uint32Array <$ reserved "Uint32Array"
    <|> Uint8ClampedArray <$ reserved "Uint8ClampedArray"
    <|> BigInt64Array <$ reserved "BigInt64Array"
    <|> BigUint64Array <$ reserved "BigUint64Array"
    <|> Float32Array <$ reserved "Float32Array"
    <|> Float64Array <$ reserved "Float64Array"

stringTypeP :: Parser StringType
stringTypeP =
  ByteString <$ reserved "ByteString"
    <|> DOMString <$ reserved "DOMString"
    <|> USVString <$ reserved "USVString"

extendedAttributeListP :: Parser (V.Vector ExtendedAttribute)
extendedAttributeListP =
  P.option V.empty $
    brackets $
      V.fromList <$> extendedAttributeP `P.sepBy1` comma

extendedAttributeP :: Parser ExtendedAttribute
extendedAttributeP = do
  lhs <- anyIdentifier
  P.option (ExtendedAttributeNoArgs lhs) $
    ExtendedAttributeArgList lhs <$> parens argumentListP
      <|> equals
        *> ( ExtendedAttributeWildcard lhs <$ symbol "*"
              <|> ExtendedAttributeIdent lhs <$> anyString
              <|> do
                rhs <- anyIdentifier
                P.option
                  (ExtendedAttributeIdent lhs rhs)
                  ( parens $
                      ExtendedAttributeNamedArgList lhs rhs <$> argumentListP
                  )
              <|> ExtendedAttributeIdentList lhs <$> parens (V.fromList <$> anyIdentifier `P.sepBy1` comma)
           )

keywords :: HS.HashSet T.Text
keywords =
  HS.fromList
    [ "ArrayBuffer"
    , "async"
    , "attribute"
    , "any"
    , "bigint"
    , "BigInt64Array"
    , "BigUint64Array"
    , "boolean"
    , "byte"
    , "ByteString"
    , "callback"
    , "const"
    , "constructor"
    , "DataView"
    , "deleter"
    , "dictionary"
    , "DOMString"
    , "enum"
    , "false"
    , "Float32Array"
    , "Float64Array"
    , "FrozenArray"
    , "getter"
    , "includes"
    , "Infinity"
    , "inherit"
    , "Int16Array"
    , "Int32Array"
    , "Int8Array"
    , "interface"
    , "iterable"
    , "long"
    , "maplike"
    , "mixin"
    , "namespace"
    , "NaN"
    , "null"
    , "or"
    , "object"
    , "octet"
    , "optional"
    , "partial"
    , "readonly"
    , "record"
    , "required"
    , "sequence"
    , "setlike"
    , "setter"
    , "SharedArrayBuffer"
    , "short"
    , "static"
    , "stringifier"
    , "symbol"
    , "true"
    , "typedef"
    , "Uint16Array"
    , "Uint32Array"
    , "Uint8Array"
    , "Uint8ClampedArray"
    , "undefined"
    , "unrestricted"
    , "unsigned"
    , "USVString"
    , "Promise"
    ]
