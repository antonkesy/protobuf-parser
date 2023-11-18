module Unit.Enum (allTests) where

import Data.Either (fromRight, isRight)
import ProtoParser.Enum
import Protobuf
import Test.HUnit
import Text.Parsec (parse)

allTests :: [Test]
allTests =
  [ TestLabel "enumFieldParser" testEnumFieldParser,
    TestLabel "enumParser" testEnumParser,
    TestLabel "reservedNumbers" testReservedNumbers,
    TestLabel "fieldNumbers" testEnumFieldNumbers 
  ]

----------------------------------------------------------------
testReservedNumbers :: Test
testReservedNumbers = TestCase $ do
  assertEqual "empty" False (isRight (parse reservedNumbers "" ""))
  assertEqual "single" (Numbers [0]) (fromRight (Numbers []) (parse reservedNumbers "" "0"))
  assertEqual "range" (Numbers [0, 1, 2]) (fromRight (Numbers []) (parse reservedNumbers "" "min to 2"))

----------------------------------------------------------------

emptyDefault :: Maybe EnumField
emptyDefault = Just (EnumValue "TestDefault" 0)

testEnumFieldParser :: Test
testEnumFieldParser = TestCase $ do
  assertEqual "empyt" False (isRight (parse enumField "" ""))
  assertEqual "enumEntry" (Just (EnumValue "TEST" 0)) (fromRight emptyDefault (parse enumField "" "TEST = 0"))
  assertEqual "enumEntry" (Just (EnumValue "MORE" 1)) (fromRight emptyDefault (parse enumField "" "MORE = 1"))
  assertEqual "enumEntry" (Just (EnumValue "UNDER_SCORE" 42)) (fromRight emptyDefault (parse enumField "" "UNDER_SCORE = 42"))
  -- reserved number --
  assertEqual "empytReserved" False (isRight (parse enumField "" "reserved"))
  assertEqual "outOfRangeSingleReserved" False (isRight (parse enumField "" "reserved -1"))
  assertEqual "multiReserved" (Just (EnumReserved (Numbers [1, 2]))) (fromRight emptyDefault (parse enumField "" "reserved 1, 2"))
  assertEqual "multiReserved" (Just (EnumReserved (Numbers [1, 3, 5]))) (fromRight emptyDefault (parse enumField "" "reserved 1, 3, 5"))
  assertEqual "multiReserved" (Just (EnumReserved (Numbers [1, 2, 3]))) (fromRight emptyDefault (parse enumField "" "reserved 1 to 3"))
  assertEqual "multiReserved" (Just (EnumReserved (Numbers [0, 1, 2, 3]))) (fromRight emptyDefault (parse enumField "" "reserved min to 3"))
  assertEqual "multiReserved" (Just (EnumReserved (Numbers [4294967294, 0xFFFFFFFF]))) (fromRight emptyDefault (parse enumField "" "reserved 4294967294 to max"))
  assertEqual "singleReserved" (Just (EnumReserved (Numbers [0]))) (fromRight emptyDefault (parse enumField "" "reserved 0"))
  assertEqual "singleReserved" (Just (EnumReserved (Numbers [1]))) (fromRight emptyDefault (parse enumField "" "reserved 1"))
  assertEqual "reservedIncorrectNumberFormat" False (isRight (parse enumField "" "reserved 1 2"))
  -- reserved name --
  assertEqual "emptyReservedName" False (isRight (parse enumField "" "reserved"))
  assertEqual "singleReservedName" (Just (EnumReserved (Names ["FOO"]))) (fromRight emptyDefault (parse enumField "" "reserved \"FOO\""))
  assertEqual "multiReservedName" (Just (EnumReserved (Names ["FOO", "BAR"]))) (fromRight emptyDefault (parse enumField "" "reserved \"FOO\", \"BAR\""))
  -- option --
  assertEqual "empyt" False (isRight (parse enumField "" "option invalid_option = true"))
  assertEqual "invalidOption" (Just (EnumOption "allow_alias" True)) (fromRight emptyDefault (parse enumField "" "option allow_alias = true"))
  assertEqual "invalidOption" (Just (EnumOption "allow_alias" False)) (fromRight emptyDefault (parse enumField "" "option allow_alias = false"))

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
  assertEqual "empyt" False (isRight (parse protoEnum "" ""))
  assertEqual "atLeastOneEnumField" False (isRight (parse protoEnum "" "enum Test{}"))
  assertEqual "singleEnum" (Protobuf.Enum "Test" [EnumValue "A" 0]) (fromRight empytDefault (parse protoEnum "" "enum Test { A = 0; }"))
  assertEqual "multiple" exampleEnumField (fromRight empytDefault (parse protoEnum "" exampleEnum))

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
