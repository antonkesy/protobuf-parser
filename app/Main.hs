module Main (main) where

import ProtoParser
import ProtoParser.Package
import Text.Parsec (parse)

main :: IO ()
main = do
  case parse enumField "" "reserved 1 2" of
    Left err -> print err
    Right res -> print res
