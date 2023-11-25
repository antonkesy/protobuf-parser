module Unit.Enum (allTests) where

import Data.Either (fromRight, isRight)
import ProtoParser.Enum
import Protobuf
import Test.HUnit
import Text.Parsec (parse)

allTests :: [Test]
allTests =
  [ TestLabel "parseEnumFieldParser" testEnumFieldParser,
    TestLabel "enumParser" testEnumParser,
    TestLabel "reservedEnumNumbers" testReservedEnumNumbers,
    TestLabel "fieldNumbers" testEnumFieldNumbers
  ]

----------------------------------------------------------------
testReservedEnumNumbers :: Test
testReservedEnumNumbers = TestCase $ do
  assertEqual "empty" False (isRight (parse (reservedNumbers enumNumber enumNumberRange) "" ""))
  assertEqual "single" [0] (fromRight [] (parse (reservedNumbers enumNumber enumNumberRange) "" "0"))
  assertEqual "range" [0, 1, 2] (fromRight [] (parse (reservedNumbers enumNumber enumNumberRange) "" "min to 2"))

----------------------------------------------------------------

emptyDefault :: EnumField
emptyDefault = EnumValue "TestDefault" 0

testEnumFieldParser :: Test
testEnumFieldParser = TestCase $ do
  assertEqual "empty" False (isRight (parse parseEnumField "" ""))
  assertEqual "enumEntry" (EnumValue "TEST" 0) (fromRight emptyDefault (parse parseEnumField "" "TEST = 0"))
  assertEqual "enumEntry" (EnumValue "MORE" 1) (fromRight emptyDefault (parse parseEnumField "" "MORE = 1"))
  assertEqual "enumEntry" (EnumValue "UNDER_SCORE" 42) (fromRight emptyDefault (parse parseEnumField "" "UNDER_SCORE = 42"))
  -- reserved number --
  assertEqual "empytReserved" False (isRight (parse parseEnumField "" "reserved"))
  assertEqual "outOfRangeSingleReserved" False (isRight (parse parseEnumField "" "reserved -1"))
  assertEqual "multiReserved" (EnumReserved (ReservedEnumNumbers [1, 2])) (fromRight emptyDefault (parse parseEnumField "" "reserved 1, 2"))
  assertEqual "multiReserved" (EnumReserved (ReservedEnumNumbers [1, 3, 5])) (fromRight emptyDefault (parse parseEnumField "" "reserved 1, 3, 5"))
  assertEqual "multiReserved" (EnumReserved (ReservedEnumNumbers [1, 2, 3])) (fromRight emptyDefault (parse parseEnumField "" "reserved 1 to 3"))
  assertEqual "multiReserved" (EnumReserved (ReservedEnumNumbers [0, 1, 2, 3])) (fromRight emptyDefault (parse parseEnumField "" "reserved min to 3"))
  assertEqual "multiReserved" (EnumReserved (ReservedEnumNumbers [4294967294, 0xFFFFFFFF])) (fromRight emptyDefault (parse parseEnumField "" "reserved 4294967294 to max"))
  assertEqual "singleReserved" (EnumReserved (ReservedEnumNumbers [0])) (fromRight emptyDefault (parse parseEnumField "" "reserved 0"))
  assertEqual "singleReserved" (EnumReserved (ReservedEnumNumbers [1])) (fromRight emptyDefault (parse parseEnumField "" "reserved 1"))
  -- assertEqual "reservedIncorrectNumberFormat" False (isRight (parse parseEnumField "" "reserved 1 2")) -- cant parse with enumField alone anymore
  -- reserved name --
  assertEqual "emptyReservedName" False (isRight (parse parseEnumField "" "reserved"))
  assertEqual "singleReservedName" (EnumReserved (ReservedEnumNames (ReservedNames ["FOO"]))) (fromRight emptyDefault (parse parseEnumField "" "reserved \"FOO\""))
  assertEqual "multiReservedName" (EnumReserved (ReservedEnumNames (ReservedNames ["FOO", "BAR"]))) (fromRight emptyDefault (parse parseEnumField "" "reserved \"FOO\", \"BAR\""))
  -- option --
  assertEqual "empty" False (isRight (parse parseEnumField "" ""))
  assertEqual "invalidOption" (EnumOption "allow_alias" True) (fromRight emptyDefault (parse parseEnumField "" "option allow_alias = true"))
  assertEqual "invalidOption" (EnumOption "allow_alias" False) (fromRight emptyDefault (parse parseEnumField "" "option allow_alias = false"))

----------------------------------------------------------------

empytDefault :: Protobuf.Enum
empytDefault = Protobuf.Enum "TestDefault" []

exampleEnum :: String
exampleEnum =
  "enum TestEnum {\n\
  \  UNKNOWN = 0;\n\
  \  STARTED = 1;\n\
  \  RUNNING = 1;\n\
  \}\n"

exampleEnumField :: Protobuf.Enum
exampleEnumField = Protobuf.Enum "TestEnum" [EnumValue "UNKNOWN" 0, EnumValue "STARTED" 1, EnumValue "RUNNING" 1]

testEnumParser :: Test
testEnumParser = TestCase $ do
  assertEqual "empty" False (isRight (parse parseEnum "" ""))
  assertEqual "atLeastOneEnumField" False (isRight (parse parseEnum "" "enum Test{}"))
  assertEqual "singleEnum" (Protobuf.Enum "Test" [EnumValue "A" 0]) (fromRight empytDefault (parse parseEnum "" "enum Test { A = 0; }"))
  assertEqual "multiple" exampleEnumField (fromRight empytDefault (parse parseEnum "" exampleEnum))

----------------------------------------------------------------

-- TODO: test enum with options
-- enum Data {
--   DATA_UNSPECIFIED = 0;
--   DATA_SEARCH = 1 [deprecated = true];
--   DATA_DISPLAY = 2 [
--     (string_name) = "display_value"
--   ];
-- }

testEnumFieldNumbers :: Test
testEnumFieldNumbers = TestCase $ do
  assertEqual "belowMin" False (isRight (parse enumNumber "" "-1"))
  assertEqual "min" 0 (fromRight 1 (parse enumNumber "" "0"))
  assertEqual "max" 0xFFFFFFFF (fromRight 0 (parse enumNumber "" "4294967295"))

-- TODO: not correct number
-- assertEqual "aboveMax" (False) (isRight (parse enumNumber "" "4294967296"))
----------------------------------------------------------------
