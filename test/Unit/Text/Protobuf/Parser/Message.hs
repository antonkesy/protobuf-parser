module Unit.Text.Protobuf.Parser.Message (allTests) where

import Data.Either (fromRight, isRight)
import Test.HUnit
import Text.Parsec (parse)
import Text.Protobuf.Parser.Message
import Text.Protobuf.Types

allTests :: [Test]
allTests =
  [TestLabel "simple" testSimple]

failMessage :: Message
failMessage = Message "FAIL" []

testMessage1 :: String
testMessage1 =
  "message Foo {\
  \int32 foo = 1;\
  \double bar = 2;\
  \}"

testMessage1Proto :: Message
testMessage1Proto =
  Message
    "Foo"
    [ ImplicitMessageField (Scalar (IntType Int32)) "foo" 1 [],
      ImplicitMessageField (Scalar (FloatType Double)) "bar" 2 []
    ]

testMessageReserved :: String
testMessageReserved =
  "message Foo {\
  \reserved 1, 2;\
  \}"

testMessageReservedProto :: Message
testMessageReservedProto =
  Message
    "Foo"
    [ MessageReserved (ReservedMessageNumbers [1, 2])
    ]

testOptional :: String
testOptional =
  "message Foo {\
  \optional int32 foo = 1;\
  \}"

testOptionalProto :: Message
testOptionalProto =
  Message
    "Foo"
    [ OptionalMessageField (Scalar (IntType Int32)) "foo" 1 []
    ]

testRepeated :: String
testRepeated =
  "message Foo {\
  \repeated int32 foo = 1;\
  \}"

testRepeatedProto :: Message
testRepeatedProto =
  Message
    "Foo"
    [ RepeatedMessageField (Scalar (IntType Int32)) "foo" 1 []
    ]

testReservedNames :: String
testReservedNames =
  "message Foo {\
  \reserved \"foo\", \"bar\";\
  \}"

testReservedNamesProto :: Message
testReservedNamesProto =
  Message
    "Foo"
    [ MessageReserved (ReservedMessageNames (ReservedNames ["foo", "bar"]))
    ]

testOneOf :: String
testOneOf =
  "message Foo {\
  \oneof foo {\
  \int32 bar = 1;\
  \double baz = 2;\
  \}\
  \}"

testOneOfProto :: Message
testOneOfProto =
  Message
    "Foo"
    [ OneOfMessageField
        "foo"
        [ ImplicitMessageField (Scalar (IntType Int32)) "bar" 1 [],
          ImplicitMessageField (Scalar (FloatType Double)) "baz" 2 []
        ]
    ]

testFieldOption :: String
testFieldOption =
  "message Foo {\
  \int32 foo = 1 [default = true];\
  \}"

testFieldOptionProto :: Message
testFieldOptionProto =
  Message
    "Foo"
    [ ImplicitMessageField
        (Scalar (IntType Int32))
        "foo"
        1
        [FieldOption "default" (ConstantBoolLit True)]
    ]

testSimple :: Test
testSimple = TestCase $ do
  assertEqual "empty" False (isRight (parse parseMessage "" ""))
  assertEqual "keyword only" False (isRight (parse parseMessage "" "message"))
  assertEqual "missing name" False (isRight (parse parseMessage "" "message {}"))
  assertEqual "emptyMessage" (Message "Foo" []) (fromRight failMessage (parse parseMessage "" "message Foo {}"))
  assertEqual "simple" testMessage1Proto (fromRight failMessage (parse parseMessage "" testMessage1))
  assertEqual "reserved" testMessageReservedProto (fromRight failMessage (parse parseMessage "" testMessageReserved))
  assertEqual "optional" testOptionalProto (fromRight failMessage (parse parseMessage "" testOptional))
  assertEqual "repeated" testRepeatedProto (fromRight failMessage (parse parseMessage "" testRepeated))
  assertEqual "reserved names" testReservedNamesProto (fromRight failMessage (parse parseMessage "" testReservedNames))
  assertEqual "oneof" testOneOfProto (fromRight failMessage (parse parseMessage "" testOneOf))
  assertEqual "field option" testFieldOptionProto (fromRight failMessage (parse parseMessage "" testFieldOption))
