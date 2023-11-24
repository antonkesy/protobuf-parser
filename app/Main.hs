module Main (main) where

import ProtoParser

erro :: String
erro =
  "package foo;\n\
  \message B {}\n\
  \package bar;"

main :: IO ()
main = do
  case parseProtobuf erro of
    Left err -> print err
    Right res -> print res
