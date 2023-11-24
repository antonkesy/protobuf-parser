module Main (main) where

import ProtoParser
import Text.Parsec (parse)

-- erro :: String
-- erro =
--   "message Foo {\
--   \int32 foo = 1;\
--   \int32 bar = 2;\
--   \}"

-- erro =
  -- "message Foo {\
  -- \int32 bar = 2;\
  -- \}"

main :: IO ()
main = do
  -- case parseProtobuf erro of {}
  case parse enumField "" "reserved 1, 2" of
    Left err -> print err
    Right res -> print res
