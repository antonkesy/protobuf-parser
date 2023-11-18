module Main (main) where

import ProtoParser
import ProtoParser.Package
import Text.Parsec (parse)

main :: IO ()
main = do
  -- case parse enumField "" "reserved \"FOO\",\"FOO\"" of
  -- case parse enumField "" "reserved 1" of
  case parse parseMessage "" "message Foo {}" of
    -- case parse enumField "" "reserved 4294967294 to max" of
    Left err -> print err
    Right res -> print res
