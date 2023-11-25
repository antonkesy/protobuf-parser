module Main (main) where

-- import ProtoParser.Enum
import ProtoParser

-- import Text.Parsec (parse)

-- erro :: String
-- erro =
--   "message Foo {\
--   \int32 foo = 1;\
--   \int32 bar = 2;\
--   \reserved 1, 2;\
--   \}"

-- testMessageReserved :: String
-- testMessageReserved = "syntax = \"proto3\";"

testMessageReserved :: String
testMessageReserved =
  "import \"foo.proto\";\n\
  \package foobar;\n\
  \import \"bar.proto\";"

main :: IO ()
main = do
  case parseProtobuf testMessageReserved of
    Left err -> print err
    Right protobuf -> print protobuf

-- main :: IO ()
-- main = do
--   result <- parseProtoFile "test/protofiles/chat.proto"
--   case result of
--     Left err -> putStrLn $ "Parse error: " ++ show err
--     Right protobuf -> putStrLn $ "Successfully parsed: " ++ show protobuf
