# protobuf-parser

Simplified [Protocol Buffers]\[proto 3](https://protobuf.dev/programming-guides/proto3/) and [gRPC](https://grpc.io/docs/what-is-grpc/introduction/) parser using Haskell and [Parsec](https://hackage.haskell.org/package/parsec)

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
stack run "import "foo.proto"; import "bar.proto"; package foobar;"
stack run -- -p "import "foo.proto"; import "bar.proto"; package foobar;"

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

## Grammar

TODO: past into parser segments

## Simplifications

This projects acts as a parser combinator showcase project.
Therefore, not all features are complete or correct:

- Only proto3 syntax is supported
- Not all values are check for correctness
- Base Lexical Elements do not strictly follow the [offical spec](https://protobuf.dev/reference/protobuf/proto3-spec/#lexical_elements)
- Protobuf 3 Ranges do not allow the keyword "min"
- Empty statements are missing
