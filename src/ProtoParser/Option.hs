module ProtoParser.Option (parseOption, parseOption') where

import ProtoParser.Space (spaces', spaces1)
import ProtoParser.Type (protoName)
import Protobuf
import Text.Parsec
import Text.Parsec.String

parseOption' :: Protobuf -> Parser Protobuf
parseOption' p = do
  opt <- parseOption
  return
    ( Protobuf.merge
        p
        (Protobuf {syntax = Nothing, package = Nothing, imports = [], options = [opt], enums = [], messages = [], services = []})
    )

parseOption :: Parser Option
parseOption =
  Option
    <$> (spaces' *> string "option" *> spaces1 *> protoName <* spaces1)
    <*> (spaces' *> char '=' *> spaces' *> char '\"' *> (anyChar `manyTill` char '"') <* spaces' <* char ';')
