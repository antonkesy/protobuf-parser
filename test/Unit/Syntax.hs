module Unit.Syntax (allTests) where

import Data.Either (fromRight, isRight)
import ProtoParser.Syntax
import Protobuf
import Test.HUnit
import Text.Parsec (parse)

allTests :: [Test]
allTests =
  [TestLabel "syntax" testSyntax]

testSyntax :: Test
testSyntax = TestCase $ do
  assertEqual "empty" False (isRight (parse parseSyntax "" ""))
  assertEqual "missing package name" False (isRight (parse parseSyntax "" "syntax"))
  assertEqual "missing ';'" False (isRight (parse parseSyntax "" "syntax \"proto3\""))
  assertEqual "invalid Proto Version" False (isRight (parse parseSyntax "" "syntax = \"proto1\";"))
  assertEqual "Proto2" True (isRight (parse parseSyntax "" "syntax = \"proto2\";"))
  assertEqual "Proto2" Proto2 (fromRight Proto3 (parse parseSyntax "" "syntax = \"proto2\";"))
  assertEqual "Proto3" Proto3 (fromRight Proto2 (parse parseSyntax "" "syntax = \"proto3\";"))
