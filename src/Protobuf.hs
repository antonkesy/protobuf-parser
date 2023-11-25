{-# LANGUAGE GADTs #-}

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

data MessageField
  = ImplicitMessageField DataType Name FieldNumber
  | OptionalMessageField DataType Name FieldNumber
  | RepeatedMessageField DataType Name FieldNumber
  | MessageReserved MessageReservedValues
  | OneOfMessageField Name [MessageField]
  deriving (Show, Eq)

data Message
  = Message MessageName [MessageField]
  deriving (Show, Eq)

data ReservedNames where
  ReservedNames :: [Name] -> ReservedNames
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
  = EnumValue Name EnumNumber
  | EnumOption Name Bool
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
  = Option Name Value
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
    enums :: [Protobuf.Enum],
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
merge' = foldl1 Protobuf.merge

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
