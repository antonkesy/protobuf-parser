module ProtoParser.Reserved
  ( module ProtoParser.Reserved,
  )
where

import ProtoParser.Space (spaces')
import ProtoParser.Type
import Protobuf
import Text.Parsec
import Text.Parsec.String

-- https://protobuf.dev/programming-guides/proto3/#reserved

----------------------------------------------------------------

reservedNames :: Parser ReservedNames
reservedNames = do
  names <- try reservedNames' `sepBy1` char ','
  return (ReservedNames (concat names))

reservedNames' :: Parser [Name]
reservedNames' = do
  _ <- spaces'
  _ <- char '\"'
  name <- protoName
  _ <- char '\"'
  return [name]

----------------------------------------------------------------

reservedNumbersSingle :: Parser a -> Parser [a]
reservedNumbersSingle p = do
  _ <- spaces'
  firstNumber <- p
  _ <- spaces'
  return [firstNumber]

reservedNumbersRange :: (Integral a) => Parser a -> Parser [a]
reservedNumbersRange range = do
  firstNumber <- range
  _ <- spaces'
  _ <- string "to"
  _ <- spaces'
  secondNumber <- range
  return [firstNumber .. secondNumber]

reservedNumbers :: (Integral a) => Parser a -> Parser a -> Parser [a]
reservedNumbers single range = do
  numbers <- try (reservedNumbers' single range) `sepBy1` char ','
  return ((concat numbers))

reservedNumbers' :: (Integral a) => Parser a -> Parser a -> Parser [a]
reservedNumbers' single range = try (reservedNumbersRange range) <|> try (reservedNumbersSingle single)
