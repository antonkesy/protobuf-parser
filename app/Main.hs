module Main (main) where

import ProtoParser.Message
import Text.Parsec (parse)

-- erro :: String
-- erro =
--   "message Foo {\
--   \int32 foo = 1;\
--   \int32 bar = 2;\
--   \reserved 1, 2;\
--   \}"

testMessageReserved :: String
testMessageReserved =
  "message Foo {\
  \reserved \"foo\", \"bar\";\
  \}"

main :: IO ()
main = do
  case parse parseMessage "" testMessageReserved of
    Left err -> print err
    Right protobuf -> print protobuf

-- main :: IO ()
-- main = do
--   result <- parseProtoFile "example.proto"
--   case result of
--     Left err -> putStrLn $ "Parse error: " ++ show err
--     Right protobuf -> putStrLn $ "Successfully parsed: " ++ show protobuf
