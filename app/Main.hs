module Main (main) where

import ProtoParser

-- import Text.Parsec (parse)

-- erro :: String
-- erro =
--   "message Foo {\
--   \int32 foo = 1;\
--   \int32 bar = 2;\
--   \reserved 1, 2;\
--   \}"

main :: IO ()
main = do
  result <- parseProtoFile "example.proto"
  case result of
    Left err -> putStrLn $ "Parse error: " ++ show err
    Right protobuf -> putStrLn $ "Successfully parsed: " ++ show protobuf
