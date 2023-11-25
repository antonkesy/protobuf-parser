module ProtoParser.Service (parseService, parseService') where

import ProtoParser.Space (spaces', spaces1)
import ProtoParser.Type
import Protobuf
import Text.Parsec
import Text.Parsec.String

parseService' :: Protobuf -> Parser Protobuf
parseService' p = do
  x <- parseService
  return
    ( Protobuf.merge
        p
        (Protobuf {syntax = Nothing, package = Nothing, imports = [], options = [], enums = [], messages = [], services = [x]})
    )

parseService :: Parser Service
parseService =
  Service
    <$> ( spaces'
            *> string "service"
            *> spaces1
            *> protoName
        )
    <*> ( spaces'
            *> char '{'
            *> spaces'
            *> (try parseServiceField `sepEndBy1` (lookAhead anyChar))
            <* spaces'
            <* char '}'
        )

parseServiceField :: Parser RPC
parseServiceField =
  RPC
    <$> (spaces' *> string "rpc" *> spaces1 *> protoName)
    <*> ( spaces'
            *> char '('
            *> spaces'
            *> (try requestStream <|> request)
            <* spaces'
            <* char ')'
        )
    <*> ( spaces'
            *> string "returns"
            *> spaces'
            *> char '('
            *> spaces'
            *> (try replyStream <|> reply)
            <* spaces'
            <* char ')'
        )
    <* spaces'
    <* char '{'
    <* spaces'
    <* char '}'
    <* spaces'
  where
    request = RequestType <$> protoName
    requestStream = string "stream" *> spaces1 *> (RequestTypeStream <$> protoName)
    reply = ReplyType <$> protoName
    replyStream = string "stream" *> spaces1 *> (ReplyTypeStream <$> protoName)
