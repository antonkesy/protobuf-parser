module Main (main) where

import ProtoParser

splitImportText1 :: String
splitImportText1 =
  "import \"foo.proto\";\n\
  \message FooBar {}\n\
  \import \"bar.proto\";"

main :: IO ()
main = do
  case parseProtobuf splitImportText1 of
    Left err -> print err
    Right res -> print res
