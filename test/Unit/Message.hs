module Unit.Message (allTests) where

import Data.Either (fromRight, isRight)
import ProtoParser.Message
import Protobuf
import Test.HUnit
import Text.Parsec (parse)

allTests :: [Test]
allTests =
  [ TestLabel "simple" testSimple
  ]

failMessage :: Message
failMessage = Message "FAIL" []

testSimple :: Test
testSimple = TestCase $ do
  assertEqual "empty" False (isRight (parse parseMessage "" ""))
  assertEqual "keyword only" False (isRight (parse parseMessage "" "message"))
  assertEqual "missing name" False (isRight (parse parseMessage "" "message {}"))
  assertEqual "emptyMessage" (Message "Foo" []) (fromRight failMessage (parse parseMessage "" "message Foo {}"))
