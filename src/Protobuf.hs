module Protobuf (module Protobuf) where

import Data.Word (Word32)

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

data ScalarType = IntType IntType | FloatType FloatType | String | Bytes | Bool
  deriving (Show, Eq)

data MapKey = StringKey String | IntKey IntType
  deriving (Show, Eq)

data MapValue = MapName | ProtonScalarType
  deriving (Show, Eq)

data ProtoDataType = ProtoScalarType | MessageName | EnumName | Map MapKey MapValue
  deriving (Show, Eq)

data MessageField = MessageField ProtoDataType Name FieldNumber Repeat
  deriving (Show, Eq)

data Message = Message MessageName [MessageField]
  deriving (Show, Eq)

data EnumReservedValues = Numbers [EnumNumber] | Names [Name]
  deriving (Show, Eq)

data EnumField = EnumValue Name EnumNumber | EnumOption Name Bool | EnumReserved EnumReservedValues
  deriving (Show, Eq)

data Enum = Enum EnumName [EnumField]
  deriving (Show, Eq)

data Service = Service Name [RPC]
  deriving (Show, Eq)

data RequestType = RequestType MessageName | RequestTypeStream MessageName
  deriving (Show, Eq)

data ReplyType = ReplyType MessageName | ReturnTypeStream MessageName
  deriving (Show, Eq)

data RPC = RPC RPCName RequestType ReplyType
  deriving (Show, Eq)

data Option = Option Name Value
  deriving (Show, Eq)

data Protobuf = Protobuf
  -- { package :: Maybe String,
  { package :: [String],
    imports :: [ImportPath],
    options :: [Option],
    enums :: [Protobuf.Enum],
    messages :: [Message],
    services :: [Service]
  }
  deriving (Show, Eq)

------------------------------------------------------------

merge' :: [Protobuf] -> Protobuf
merge' = foldl1 Protobuf.merge

merge :: Protobuf -> Protobuf -> Protobuf
merge a b =
  Protobuf
    { package = package a ++ package b,
      imports = imports a ++ imports b,
      options = options a ++ options b,
      enums = enums a ++ enums b,
      messages = messages a ++ messages b,
      services = services a ++ services b
    }
