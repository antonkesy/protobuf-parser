module Text.Protobuf.Types (module Text.Protobuf.Types) where

import Data.Word (Word32)
import Prettyprinter

type FieldNumber = Int

type Repeat = Bool

type EnumNumber = Word32

type Name = String

type Value = String

type MessageName = String

type EnumName = String

type RPCName = String

type ImportPath = String

type Comment = String

type Package = String

data IntType
  = Int32
  | Int64
  | UInt32
  | UInt64
  | SInt32
  | SInt64
  | Fixed32
  | Fixed64
  | SFixed32
  | SFixed64
  deriving (Show, Eq)

data FloatType
  = Double
  | Float
  deriving (Show, Eq)

data ScalarType
  = IntType IntType
  | FloatType FloatType
  | StringType
  | BytesType
  | BoolType
  deriving (Show, Eq)

data MapKey
  = StringKey String
  | IntKey IntType
  deriving (Show, Eq)

data MapValue
  = MapName String
  | ScalarType
  deriving (Show, Eq)

data DataType
  = Scalar ScalarType
  | Compound Name
  | Map MapKey MapValue
  deriving (Show, Eq)

data OptionValue
  = StringValue String
  | BoolValue Bool
  | CompoundValue Name
  deriving (Show, Eq)

data FieldOption
  = FieldOption Name OptionValue
  | MultipleFieldOption [FieldOption]
  deriving (Show, Eq)

data MessageField
  = ImplicitMessageField DataType Name FieldNumber [FieldOption]
  | OptionalMessageField DataType Name FieldNumber [FieldOption]
  | RepeatedMessageField DataType Name FieldNumber [FieldOption]
  | MessageReserved MessageReservedValues
  | OneOfMessageField Name [MessageField]
  | EnumMessageField Text.Protobuf.Types.Enum
  | OptionMessageField Option
  deriving (Show, Eq)

data Message
  = Message MessageName [MessageField]
  deriving (Show, Eq)

data ReservedNames = ReservedNames [Name]
  deriving (Show, Eq)

data MessageReservedValues
  = ReservedMessageNumbers [FieldNumber]
  | ReservedMessageNames ReservedNames
  deriving (Show, Eq)

-- TODO: make reserved type generic
data EnumReservedValues
  = ReservedEnumNumbers [EnumNumber]
  | ReservedEnumNames ReservedNames
  deriving (Show, Eq)

data EnumField
  = EnumValue Name EnumNumber [FieldOption]
  | EnumOption Option
  | EnumReserved EnumReservedValues
  deriving (Show, Eq)

data Enum
  = Enum EnumName [EnumField]
  deriving (Show, Eq)

data Service
  = Service Name [RPC]
  deriving (Show, Eq)

data RequestType
  = RequestType MessageName
  | RequestTypeStream MessageName
  deriving (Show, Eq)

data ReplyType
  = ReplyType MessageName
  | ReplyTypeStream MessageName
  deriving (Show, Eq)

data RPC
  = RPC RPCName RequestType ReplyType
  deriving (Show, Eq)

data Option
  = Option Name OptionValue
  deriving (Show, Eq)

data Syntax
  = Proto2
  | Proto3
  deriving (Show, Eq)

data Protobuf = Protobuf
  { syntax :: Maybe Syntax,
    package :: Maybe String,
    imports :: [ImportPath],
    options :: [Option],
    enums :: [Text.Protobuf.Types.Enum],
    messages :: [Message],
    services :: [Service]
  }
  deriving (Show, Eq)

------------------------------------------------------------
emptyProtobuf :: Protobuf
emptyProtobuf =
  ( Protobuf
      { package = Nothing,
        imports = [],
        syntax = Nothing,
        options = [],
        enums = [],
        messages = [],
        services = []
      }
  )

------------------------------------------------------------

merge' :: [Protobuf] -> Protobuf
merge' = foldl1 Text.Protobuf.Types.merge

merge :: Protobuf -> Protobuf -> Protobuf
merge a b =
  Protobuf
    { syntax = mergeSyntax (syntax a) (syntax b),
      package = mergePackages (package a) (package b),
      imports = imports a ++ imports b,
      options = options a ++ options b,
      enums = enums a ++ enums b,
      messages = messages a ++ messages b,
      services = services a ++ services b
    }
  where
    mergePackages :: Maybe String -> Maybe String -> Maybe String
    mergePackages Nothing y = y
    mergePackages x Nothing = x
    mergePackages (Just x) (Just y)
      | not (null x) && not (null y) = error "Conflicting non-empty packages"
      | otherwise = Just (x ++ y)
    mergeSyntax :: Maybe Syntax -> Maybe Syntax -> Maybe Syntax
    mergeSyntax Nothing y = y
    mergeSyntax x Nothing = x
    mergeSyntax (Just x) (Just y)
      | x == y = Just x
      | otherwise = error "Conflicting syntax versions"

------------------------------------------------------------

instance Pretty Protobuf where
  pretty protobuf =
    vsep
      [ pretty "Protobuf",
        pretty "{",
        indent 2 (pretty "syntax =" <+> pretty (syntax protobuf) <> pretty ","),
        indent 2 (pretty "package =" <+> pretty (package protobuf) <> pretty ","),
        indent 2 (pretty "imports =" <+> pretty (imports protobuf) <> pretty ","),
        indent 2 (pretty "options =" <+> pretty (options protobuf) <> pretty ","),
        indent 2 (pretty "enums =" <+> pretty (enums protobuf) <> pretty ","),
        indent 2 (pretty "messages =" <+> pretty (messages protobuf) <> pretty ","),
        indent 2 (pretty "services =" <+> pretty (services protobuf)),
        pretty "}"
      ]

instance Pretty FloatType where
  pretty Double = pretty "double"
  pretty Float = pretty "float"

instance Pretty ScalarType where
  pretty (IntType intType) = pretty intType
  pretty (FloatType floatType) = pretty floatType
  pretty StringType = pretty "string"
  pretty BytesType = pretty "bytes"
  pretty BoolType = pretty "bool"

instance Pretty IntType where
  pretty Int32 = pretty "int32"
  pretty Int64 = pretty "int64"
  pretty UInt32 = pretty "uint32"
  pretty UInt64 = pretty "uint64"
  pretty SInt32 = pretty "sint32"
  pretty SInt64 = pretty "sint64"
  pretty Fixed32 = pretty "fixed32"
  pretty Fixed64 = pretty "fixed64"
  pretty SFixed32 = pretty "sfixed32"
  pretty SFixed64 = pretty "sfixed64"

instance Pretty Message where
  pretty (Message name fields) =
    vsep
      [ pretty "message"
          <+> pretty name
          <+> pretty "{",
        indent 3 (vsep (map pretty fields)),
        indent 2 (pretty "}")
      ]

instance Pretty FieldOption where
  pretty (FieldOption name value) =
    pretty name
      <+> pretty "="
      <+> pretty value
  pretty (MultipleFieldOption opt) =
    vsep
      [ pretty "[",
        indent 2 (prettyList opt),
        pretty "]"
      ]

instance Pretty OptionValue where
  pretty (StringValue s) = dquotes (pretty s)
  pretty (BoolValue b) = pretty b
  pretty (CompoundValue name) = pretty name

instance Pretty MapKey where
  pretty (StringKey s) = dquotes (pretty s)
  pretty (IntKey intType) = pretty intType

instance Pretty MapValue where
  pretty (MapName name) = pretty name
  pretty s = pretty s

instance Pretty DataType where
  pretty (Scalar st) = pretty st
  pretty (Compound name) = pretty name
  pretty (Map key value) =
    pretty "map"
      <+> pretty key
      <+> pretty "=>"
      <+> pretty value

instance Pretty MessageField where
  pretty (ImplicitMessageField dt name fieldNum opt) =
    vsep
      [ pretty dt,
        pretty name
          <+> pretty "="
          <+> pretty fieldNum
          <+> prettyList opt
      ]
  pretty (OptionalMessageField dt name fieldNum opt) =
    vsep
      [ pretty "optional"
          <+> pretty dt,
        pretty name
          <+> pretty "="
          <+> pretty fieldNum
          <+> prettyList opt
      ]
  pretty (RepeatedMessageField dt name fieldNum opt) =
    vsep
      [ pretty "repeated"
          <+> pretty dt,
        pretty name
          <+> pretty "="
          <+> pretty fieldNum
          <+> prettyList opt
      ]
  pretty (MessageReserved values) = pretty values
  pretty (OneOfMessageField name fields) =
    vsep
      [ pretty "oneof"
          <+> pretty name
          <+> pretty "{",
        indent 2 (vsep (map pretty fields)),
        pretty "}"
      ]
  pretty (OptionMessageField option) =
    pretty option
  pretty (EnumMessageField enum) =
    pretty enum

instance Pretty MessageReservedValues where
  pretty (ReservedMessageNumbers numbers) =
    pretty "reserved"
      <+> hsep (map pretty numbers)
  pretty (ReservedMessageNames (ReservedNames names)) =
    pretty "reserved"
      <+> hsep (map pretty names)

instance Pretty EnumReservedValues where
  pretty (ReservedEnumNumbers numbers) =
    pretty "reserved"
      <+> hsep (map pretty numbers)
  pretty (ReservedEnumNames (ReservedNames names)) =
    pretty "reserved"
      <+> hsep (map pretty names)

instance Pretty EnumField where
  pretty (EnumValue name number opt) =
    vsep
      [ pretty name
          <+> pretty "="
          <+> pretty number
          <+> prettyList opt
      ]
  pretty (EnumOption option) =
    pretty option
  pretty (EnumReserved values) =
    pretty values

instance Pretty Text.Protobuf.Types.Enum where
  pretty (Enum name fields) =
    vsep
      [ pretty "enum"
          <+> pretty name
          <+> pretty "{",
        indent 2 (vsep (map pretty fields)),
        pretty "}"
      ]

instance Pretty Service where
  pretty (Service name rpcs) =
    vsep
      [ pretty "service"
          <+> pretty name
          <+> pretty "{",
        indent 2 (vsep (map pretty rpcs)),
        pretty "}"
      ]

instance Pretty RequestType where
  pretty (RequestType name) =
    pretty name
  pretty (RequestTypeStream name) =
    pretty "stream"
      <+> pretty name

instance Pretty ReplyType where
  pretty (ReplyType name) =
    pretty name
  pretty (ReplyTypeStream name) =
    pretty "stream"
      <+> pretty name

instance Pretty RPC where
  pretty (RPC name reqType replyType) =
    vsep
      [ pretty "rpc"
          <+> pretty name
          <+> pretty "(",
        pretty reqType,
        pretty ")"
          <+> pretty "returns"
          <+> pretty replyType
      ]

instance Pretty Option where
  pretty (Option name value) =
    pretty name
      <+> pretty "="
      <+> pretty value

instance Pretty Syntax where
  pretty Proto2 =
    pretty "syntax = \"proto2\""
  pretty Proto3 =
    pretty "syntax = \"proto3\""
