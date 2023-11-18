module ProtoParser.Message (parseMessage) where

import ProtoParser.Misc (spaces1)
import Protobuf (Message (..), MessageField)
import Text.Parsec
import Text.Parsec.String

parseMessage :: Parser Message
parseMessage = do
  return (Message " " [])
