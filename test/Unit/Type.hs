module Unit.Type (allTests) where

import Data.Either (fromRight, isRight)
import ProtoParser.Type
import Protobuf
import Test.HUnit
import Text.Parsec (parse)

allTests :: [Test]
allTests =
  [ TestLabel "numberParser" testNumberParser,
    TestLabel "protoName" testProtoName,
    TestLabel "scalarType" testSclarType
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

testSclarType :: Test
testSclarType = TestCase $ do
  assertEqual "int32" ((IntType Int32)) (fromRight (BoolType) (parse parseScalarType "" "int32"))
  assertEqual "double" ((FloatType Double)) (fromRight (BoolType) (parse parseScalarType "" "double"))
