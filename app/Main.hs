module Main (main) where

import Prettyprinter
  ( Pretty (pretty),
    defaultLayoutOptions,
    layoutPretty,
  )
import Prettyprinter.Render.String (renderString)
import Text.Parsec (parse)
import Text.Protobuf.Parser
import Text.Protobuf.Parser.Option

testMessageReserved :: String
testMessageReserved =
  "option java_package = \"de.test\";"

main :: IO ()
main = do
  case parse option "" testMessageReserved of
    Left err -> print err
    Right protobuf -> print protobuf

-- main :: IO ()
-- main = do
--   result <- parseProtoFile "test/E2E/protofiles/chat.proto"
--   case result of
--     Left err -> putStrLn $ "Parse error: " ++ show err
--     Right protobuf ->
--       putStrLn $
--         -- renderString $
--           -- layoutPretty defaultLayoutOptions $
--             -- pretty protobuf
--             show protobuf
