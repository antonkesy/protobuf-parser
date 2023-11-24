module ProtoParser.Service (parseService, parseService') where

import Data.Maybe (catMaybes)
import ProtoParser.Misc
import ProtoParser.Space (spaces', spaces1)
import Protobuf
import Text.Parsec
import Text.Parsec.String

parseService' :: Protobuf -> Parser Protobuf
parseService' p = do
  x <- parseService
  return
    ( Protobuf.merge
        p
        (Protobuf {package = Nothing, imports = [], options = [], enums = [], messages = [], services = [x]})
    )

parseService :: Parser Service
parseService = do
  parseService''

parseService'' :: Parser Service
parseService'' = do
  spaces'
  _ <- string "service"
  spaces1
  name <- protoName
  spaces'
  _ <- char '{'
  spaces'
  fields <- try parseServiceField `sepEndBy1` char ';'
  spaces'
  _ <- char '}'
  return (Service name (catMaybes fields))

parseServiceField :: Parser (Maybe RPC)
parseServiceField = do
  spaces'
  _ <- string "rpc"
  spaces1
  name <- protoName
  spaces'
  _ <- char '('
  spaces'
  isRequestStream <- option False (string "stream" >> spaces1 >> return True)
  request <- protoName
  spaces'
  _ <- char ')'
  spaces'
  _ <- string "returns"
  spaces'
  _ <- char '('
  spaces'
  isReplyStream <- option False (string "stream" >> spaces1 >> return True)
  reply <- protoName
  spaces'
  _ <- char ')'
  spaces'
  return
    ( Just
        ( RPC
            name
            (if isRequestStream then RequestTypeStream request else RequestType request)
            (if isReplyStream then ReplyTypeStream reply else ReplyType reply)
        )
    )
