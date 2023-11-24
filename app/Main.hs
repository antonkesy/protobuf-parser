module Main (main) where

import ProtoParser
import Text.Parsec
import Text.Parsec.String

singleServiceText :: String
singleServiceText =
  "service SearchService {\n\
  \  rpc Search(SearchRequest) returns (SearchResponse);\n\
  \}"

multipleServiceText :: String
multipleServiceText =
  "service Multiple {\n\
  \  rpc Search(Foo) returns (Bar);\n\
  \  rpc Search1(Bar) returns (Foo);\n\
  \}"

main :: IO ()
main = do
  case parse parseService "" multipleServiceText of
    Left err -> print err
    Right res -> print res
