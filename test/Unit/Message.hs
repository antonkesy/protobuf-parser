module Unit.Message (allTests) where

import Data.Either (fromRight, isRight)
import ProtoParser.Message
import Protobuf (Message (..), MessageField)
import Test.HUnit
import Text.Parsec (parse)

allTests :: [Test]
allTests =
  [ TestLabel "simple" testSimple
  ]

testSimple :: Test
testSimple = TestCase $ do
  assertEqual "placeholder" True (isRight (parse parseMessage "" ""))
