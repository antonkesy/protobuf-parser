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
testOption = Option ("test") (StringValue ("fail"))

testImport :: Test
testImport = TestCase $ do
  assertEqual "empty" False (isRight (parse parseOption "" ""))
  assertEqual
    "java_package"
    (Option "java_package" (StringValue "de.test"))
    (fromRight testOption (parse parseOption "" "option java_package = \"de.test\";"))
  assertEqual
    "bool option"
    (Option "cc_enable_arenas" (BoolValue True))
    (fromRight testOption (parse parseOption "" "option cc_enable_arenas = true;"))
  assertEqual
    "compund option"
    (Option "optimize_for" (CompoundValue "SPEED"))
    (fromRight testOption (parse parseOption "" "option optimize_for = SPEED;"))
