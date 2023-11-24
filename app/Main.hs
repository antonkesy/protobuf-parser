module Main (main) where

import ProtoParser
import Text.Parsec
import Text.Parsec.String

erro :: String
erro =
  "import \"foo.proto\";\n\
  \import \"bar.proto\";"

main :: IO ()
main = do
  case parseProtobuf erro of
    -- case parse parseProtobuf "" erro of
    Left err -> print err
    Right res -> print res
