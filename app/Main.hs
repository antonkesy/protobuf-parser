module Main (main) where

import Prettyprinter
  ( Pretty (pretty),
    defaultLayoutOptions,
    layoutPretty,
  )
import Prettyprinter.Render.String (renderString)
import ProtoParser

-- import ProtoParser.Message
-- import Text.Parsec (parse)

-- testMessageReserved :: String
-- testMessageReserved =
--   "message Foo {\
--   \int32 foo = 1 [default = true];\
--   \}"

-- main :: IO ()
-- main = do
--   case parse parseMessage "" testMessageReserved of
--     Left err -> print err
--     Right protobuf -> print protobuf

main :: IO ()
main = do
  result <- parseProtoFile "test/protofiles/chat.proto"
  case result of
    Left err -> putStrLn $ "Parse error: " ++ show err
    Right protobuf ->
      putStrLn $
        renderString $
          layoutPretty defaultLayoutOptions $
            pretty protobuf
