# protobuf-parser

Simplified [Protocol Buffers Version 3](https://protobuf.dev/programming-guides/proto3/) and [gRPC](https://grpc.io/docs/what-is-grpc/introduction/) parser using Haskell and [Parsec](https://hackage.haskell.org/package/parsec)

## Usage

```bash
> stack run -- --help
Usage: protobuf-parser-exe [-f|--file PATH] [-p|--pretty] [STRING...]

Available options:
  -f,--file PATH           Specify file path to parse
  -p,--pretty              Enable pretty print
  -h,--help                Show this help text

```

```bash
stack run -- -p -f ./test/E2E/protofiles/chat.proto
stack run -- -p "import \"foo.proto\"; import \"bar.proto\"; package foobar;"
stack run "import \"foo.proto\"; import \"bar.proto\"; package foobar;"

stack test
```

## Structure

```
.
├── app
│   └── Main.hs -> CLI Parsing
├── ...
├── src
│   └── Text
│       └── Protobuf
│           ├── Parser -> Partial Parser
│           │   ├── ...
│           │   └── *.hs
│           ├── Parser.hs -> Complete Protobuf Parser
│           └── Types.hs -> Protobuf Type representation
├── ...
└── test
    ├── E2E
    │   ├── ...
    │   └── protofiles -> Example Protobuf files
    │       └── *.proto
    ├── ...
    └── Unit
        └── ...

```

## Simplifications

This projects acts as a parser combinator showcase project.
Therefore, not all features are complete or correct:

- Only proto3 syntax is supported
- Not all values are check for correctness
- Base Lexical Elements do not strictly follow the [offical spec](https://protobuf.dev/reference/protobuf/proto3-spec/#lexical_elements)
- Proto 3 Ranges do not allow the keyword "min"
- Empty statements are missing
- Import weak and public are missing

## Grammar

The correct and complete Grammar can be found at the [offical Protocol Buffers Version 3 Language Specification](https://protobuf.dev/reference/protobuf/proto3-spec/)

Following is basic syntax in Extended Backus-Naur Form (EBNF):

```
|   alternation
()  grouping
[]  option (zero or one time)
{}  repetition (any number of times)
```

```
syntax = "syntax" "=" ("'" "proto3" "'" | '"' "proto3" '"') ";"


import = "import" [ "weak" | "public" ] strLit ";"


package = "package" fullIdent ";"


constant = fullIdent | ( [ "-" | "+" ] intLit ) | ( [ "-" | "+" ] floatLit ) |
                strLit | boolLit | MessageValue


option = "option" optionName  "=" constant ";"
optionName = ( ident | "(" ["."] fullIdent ")" )


type = "double" | "float" | "int32" | "int64" | "uint32" | "uint64"
      | "sint32" | "sint64" | "fixed32" | "fixed64" | "sfixed32" | "sfixed64"
      | "bool" | "string" | "bytes" | messageType | enumType
fieldNumber = intLit;

field = [ "repeated" ] type fieldName "=" fieldNumber [ "[" fieldOptions "]" ] ";"
fieldOptions = fieldOption { ","  fieldOption }
fieldOption = optionName "=" constant

oneof = "oneof" oneofName "{" { option | oneofField } "}"
oneofField = type fieldName "=" fieldNumber [ "[" fieldOptions "]" ] ";"

mapField = "map" "<" keyType "," type ">" mapName "=" fieldNumber [ "[" fieldOptions "]" ] ";"
keyType = "int32" | "int64" | "uint32" | "uint64" | "sint32" | "sint64" |
          "fixed32" | "fixed64" | "sfixed32" | "sfixed64" | "bool" | "string"


reserved = "reserved" ( ranges | strFieldNames ) ";"
ranges = range { "," range }
range =  intLit [ "to" ( intLit | "max" ) ]
strFieldNames = strFieldName { "," strFieldName }
strFieldName = "'" fieldName "'" | '"' fieldName '"'


enum = "enum" enumName enumBody
enumBody = "{" { option | enumField | emptyStatement | reserved } "}"
enumField = ident "=" [ "-" ] intLit [ "[" enumValueOption { ","  enumValueOption } "]" ]";"
enumValueOption = optionName "=" constant


message = "message" messageName messageBody
messageBody = "{" { field | enum | message | option | oneof | mapField |
reserved | emptyStatement } "}"


service = "service" serviceName "{" { option | rpc | emptyStatement } "}"
rpc = "rpc" rpcName "(" [ "stream" ] messageType ")" "returns" "(" [ "stream" ]
messageType ")" (( "{" {option | emptyStatement } "}" ) | ";")


proto = syntax { import | package | option | topLevelDef | emptyStatement }
topLevelDef = message | enum | service
```
