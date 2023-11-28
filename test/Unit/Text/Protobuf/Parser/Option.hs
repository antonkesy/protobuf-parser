module Unit.Text.Protobuf.Parser.Option (allTests) where

import Data.Either (fromRight, isRight)
import Test.HUnit
import Text.Parsec (parse)
import Text.Protobuf.Parser.Option
import Text.Protobuf.Types

allTests :: [Test]
allTests =
  [ TestLabel "import" testImport,
    TestLabel "field option" testFieldOption
  ]

testOption :: Option
testOption = Option ("test") (ConstantStrLit ("fail"))

testImport :: Test
testImport = TestCase $ do
  assertEqual "empty" False (isRight (parse option "" ""))
  assertEqual
    "java_package"
    (Option "java_package" (ConstantStrLit "de.test"))
    (fromRight testOption (parse option "" "option java_package = \"de.test\";"))
  assertEqual
    "bool option"
    (Option "cc_enable_arenas" (ConstantBoolLit True))
    (fromRight testOption (parse option "" "option cc_enable_arenas = true;"))
  assertEqual
    "compund option"
    (Option "optimize_for" (ConstantFullIdent "SPEED"))
    (fromRight testOption (parse option "" "option optimize_for = SPEED;"))

testDefaultFieldOption :: [FieldOption]
testDefaultFieldOption = [FieldOption ("test") (ConstantStrLit ("fail"))]

testFieldOption :: Test
testFieldOption = TestCase $ do
  assertEqual "empty" False (isRight (parse parseFieldOption "" ""))
  assertEqual "missing content" False (isRight (parse parseFieldOption "" "[]"))
  assertEqual
    "single bool option"
    ([FieldOption ("deprecated") (ConstantBoolLit True)])
    (fromRight testDefaultFieldOption (parse parseFieldOption "" "[deprecated = true]"))
  assertEqual
    "multi bool option"
    ( [ (FieldOption ("deprecated") (ConstantBoolLit True)),
        (FieldOption ("other") (ConstantBoolLit False))
      ]
    )
    (fromRight testDefaultFieldOption (parse parseFieldOption "" "[deprecated = true, other = false]"))
