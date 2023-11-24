module Main (main) where

import ProtoParser

erro :: String
erro =
  "import \"foo.proto\";\n\
  \// comment\n\
  \package foobar;\n\
  \import \"bar.proto\";\
  \// comment\n"

main :: IO ()
main = do
  case parseProtobuf erro of
    Left err -> print err
    Right res -> print res
