module Unit.Option (allTests) where

import Data.Either (fromRight, isRight)
import ProtoParser.Option
import Protobuf
import Test.HUnit
import Text.Parsec (parse)

allTests :: [Test]
allTests =
  [ TestLabel "import" testImport
  ]

testOption :: Option
testOption = Option ("test") ("fail")

testImport :: Test
testImport = TestCase $ do
  assertEqual "empty" False (isRight (parse parseOption "" ""))
  assertEqual "java_package" (Option "java_package" "de.test") (fromRight testOption (parse parseOption "" "option java_package = \"de.test\";"))
