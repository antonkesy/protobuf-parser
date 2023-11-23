module Main (main) where

import ProtoParser
import Text.Parsec
import Text.Parsec.String

-- map<key_type, value_type> map_field = N;

main :: IO ()
main = do
  case parse parseMap "" "map<int32,V> name = 2" of
    Left err -> print err
    Right res -> print res
