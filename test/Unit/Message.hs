module Unit.Message (allTests) where

import Data.Either (fromRight, isRight)
import ProtoParser.Message
import Protobuf
import Test.HUnit
import Text.Parsec (parse)

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
    [ MessageField (Scalar (IntType Int32)) "foo" 1 False,
      MessageField (Scalar (FloatType Double)) "bar" 2 False
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

testSimple :: Test
testSimple = TestCase $ do
  assertEqual "empty" False (isRight (parse parseMessage "" ""))
  assertEqual "keyword only" False (isRight (parse parseMessage "" "message"))
  assertEqual "missing name" False (isRight (parse parseMessage "" "message {}"))
  assertEqual "emptyMessage" (Message "Foo" []) (fromRight failMessage (parse parseMessage "" "message Foo {}"))
  assertEqual "simple" testMessage1Proto (fromRight failMessage (parse parseMessage "" testMessage1))
  assertEqual "reserved" testMessageReservedProto (fromRight failMessage (parse parseMessage "" testMessageReserved))
