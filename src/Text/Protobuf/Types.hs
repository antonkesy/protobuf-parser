module Text.Protobuf.Types (module Text.Protobuf.Types) where

import Data.Word (Word32)

type Identifier = String

type FullIdentifier = Identifier

type MessageName = Identifier

type EnumName = Identifier

type FieldName = Identifier

type OneofName = Identifier

type MapName = Identifier

type ServiceName = Identifier

type RpcName = Identifier

type MessageType = String

type EnumType = String

type IntLit = Int

type DecimalLit = Int

type HexLit = Int

type OctalLit = Int

type FloatLit = Float

type StringLiteral = String

type StringLiteralSingle = StringLiteral

type EmptyStatement = ()

data Constant
  = ConstantFullIdent FullIdentifier
  | ConstantIntLit IntLit
  | ConstantFloatLit FloatLit
  | ConstantStrLit StringLiteral
  | ConstantBoolLit Bool
  deriving
    ( -- | ConstantMessageValue MessageValue
      Show,
      Eq
    )

-- type MessageValue = String

type OptionName = String

data Field
  = NormalField Bool FieldType Name FieldNumber [FieldOption]
  | OneOfField FieldType Name FieldNumber [FieldOption]
  | OneOf OneofName [Either Option Field] -- TODO: can only be onoffield
  | MapField MapKeyType FieldType Name FieldNumber [FieldOption]
  deriving (Show, Eq)

type MapKeyType = String -- TODO: replace string

type Range = [Int]

-----------------------
type FieldNumber = Int

type Repeat = Bool

type EnumNumber = Int

type Name = String

type Value = String

-- type MessageName = String

-- type EnumName = String

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

data FieldType
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
  = FieldOption OptionName Constant
  | MultipleFieldOption [FieldOption]
  deriving (Show, Eq)

data MessageField
  = ImplicitMessageField FieldType Name FieldNumber [FieldOption]
  | OptionalMessageField FieldType Name FieldNumber [FieldOption]
  | RepeatedMessageField FieldType Name FieldNumber [FieldOption]
  | MessageReserved MessageReservedValues
  | OneOfMessageField Name [MessageField]
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

data Reserved = Reserved (Either [Range] [Name])
  deriving (Show, Eq)

-- TODO: make reserved type generic
data EnumReservedValues
  = ReservedEnumNumbers [EnumNumber]
  | ReservedEnumNames ReservedNames
  deriving (Show, Eq)

data EnumField
  = EnumValue Name EnumNumber [FieldOption]
  | EnumOption Option
  | EnumReserved Reserved
  | EnumEmptyStatement
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
  = Option OptionName Constant
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
