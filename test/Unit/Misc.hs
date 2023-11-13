module Unit.Misc (allTests) where

import Data.Either (fromRight, isRight)
import ProtoParser
import Test.HUnit
import Text.Parsec (parse)

allTests :: [Test]
allTests =
  [ TestLabel "numberParser" testNumberParser,
    TestLabel "enumNumberParser" testEnumNumberParser,
    TestLabel "protoName" testProtoName
  ]

testNumberParser :: Test
testNumberParser = TestCase $ do
  assertEqual "nan" False (isRight (parse protoNumber "" "a"))
  assertEqual "negative" False (isRight (parse protoNumber "" "-42"))
  assertEqual "acceptedValue42" 42 (fromRight 0 (parse protoNumber "" "42"))
  -- https://protobuf.dev/programming-guides/proto3/#assigning
  -- Range from 1 to 536,870,911
  assertEqual "min" 1 (fromRight 0 (parse protoNumber "" "1"))
  assertEqual "belowMin" False (isRight (parse protoNumber "" "0"))
  assertEqual "max" 536870911 (fromRight 0 (parse protoNumber "" "536870911"))
  assertEqual "aboveMax" False (isRight (parse protoNumber "" "536870912"))
  -- 19,000 to 19,999 are reserved for the Protocol Buffers
  assertEqual "belowReserved" 18999 (fromRight 0 (parse protoNumber "" "18999"))
  assertEqual "reservedStart" False (isRight (parse protoNumber "" "19000"))
  assertEqual "inReserved" False (isRight (parse protoNumber "" "19123"))
  assertEqual "reservedEnd" False (isRight (parse protoNumber "" "19999"))
  assertEqual "aboveReserved" 20000 (fromRight 0 (parse protoNumber "" "20000"))

----------------------------------------------------------------

testProtoName :: Test
testProtoName = TestCase $ do
  assertEqual "not a name" False (isRight (parse protoName "" "-1"))
  assertEqual "Uppercase" "TEST" (fromRight "Default" (parse protoName "" "TEST"))
  assertEqual "UpperCamelCase" "TestTest" (fromRight "Default" (parse protoName "" "TestTest"))

----------------------------------------------------------------
testEnumNumberParser :: Test
testEnumNumberParser = TestCase $ do
  assertEqual "belowMin" False (isRight (parse enumNumber "" "-1"))
  assertEqual "min" 0 (fromRight 1 (parse enumNumber "" "0"))
  assertEqual "max" 0xFFFFFFFF (fromRight 0 (parse enumNumber "" "4294967295"))

-- TODO: not correct number
-- assertEqual "aboveMax" (False) (isRight (parse enumNumber "" "4294967296"))
----------------------------------------------------------------
