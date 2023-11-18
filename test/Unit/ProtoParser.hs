module Unit.ProtoParser (allTests) where

import Data.Either (fromRight, isRight)
import ProtoParser
import Protobuf
import Test.HUnit
import Text.Parsec (parse)

allTests :: [Test]
allTests =
  [ TestLabel "text" testText
  ]

testText :: Test
testText = TestCase $ do
  assertEqual "empty" False (isRight (parseProtobuf ""))
