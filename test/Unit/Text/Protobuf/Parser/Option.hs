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
testOption = Option "test" (StringValue "fail")

testImport :: Test
testImport = TestCase $ do
  assertEqual
    "empty"
    False
    ( isRight
        ( parse parseOption "" ""
        )
    )
  assertEqual
    "java_package"
    (Option "java_package" (StringValue "de.test"))
    ( fromRight
        testOption
        ( parse parseOption "" "option java_package = \"de.test\";"
        )
    )
  assertEqual
    "bool option"
    (Option "cc_enable_arenas" (BoolValue True))
    ( fromRight
        testOption
        ( parse parseOption "" "option cc_enable_arenas = true;"
        )
    )
  assertEqual
    "compund option"
    (Option "optimize_for" (CompoundValue "SPEED"))
    ( fromRight
        testOption
        (parse parseOption "" "option optimize_for = SPEED;")
    )

testDefaultFieldOption :: [FieldOption]
testDefaultFieldOption = [FieldOption "test" (StringValue "fail")]

testFieldOption :: Test
testFieldOption = TestCase $ do
  assertEqual
    "empty"
    False
    ( isRight
        (parse parseFieldOption "" "")
    )
  assertEqual
    "missing content"
    False
    (isRight (parse parseFieldOption "" "[]"))
  assertEqual
    "single bool option"
    [FieldOption "deprecated" (BoolValue True)]
    ( fromRight
        testDefaultFieldOption
        ( parse parseFieldOption "" "[deprecated = true]"
        )
    )
  assertEqual
    "multi bool option"
    [ FieldOption "deprecated" (BoolValue True),
      FieldOption "other" (BoolValue False)
    ]
    ( fromRight
        testDefaultFieldOption
        ( parse parseFieldOption "" "[deprecated = true, other = false]"
        )
    )
