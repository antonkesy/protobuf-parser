module Unit.ProtoParser (allTests) where

import Data.Either (fromRight, isRight)
import ProtoParser
import Protobuf
import Test.HUnit

allTests :: [Test]
allTests =
  [ TestLabel "text" testText,
    TestLabel "splittedDefinitions" testSplittedDefinitions
  ]

defaultTestProto :: Protobuf
defaultTestProto =
  ( Protobuf
      { package = Nothing,
        imports = [],
        options = [],
        enums = [],
        messages = [],
        services = []
      }
  )

splitImportText :: String
splitImportText =
  "import \"foo.proto\";\n\
  \package foobar;\n\
  \import \"bar.proto\";"

splitImportProto :: Protobuf
splitImportProto =
  ( Protobuf
      { package = (Just "foobar"),
        imports = ["foo.proto", "bar.proto"],
        options = [],
        enums = [],
        messages = [],
        services = []
      }
  )

splitImportText1 :: String
splitImportText1 =
  "import \"foo.proto\";\n\
  \message B {}\n\
  \import \"bar.proto\";"

splitImportProto1 :: Protobuf
splitImportProto1 =
  ( Protobuf
      { package = Nothing,
        imports = ["foo.proto", "bar.proto"],
        options = [],
        enums = [],
        messages = [Message "B" []],
        services = []
      }
  )

multiplePackageText :: String
multiplePackageText =
  "package foo;\n\
  \message B {}\n\
  \package bar;"

testSplittedDefinitions :: Test
testSplittedDefinitions = TestCase $ do
  assertEqual "import - package - import" splitImportProto (fromRight defaultTestProto (parseProtobuf splitImportText))
  assertEqual "import - message - import" splitImportProto1 (fromRight defaultTestProto (parseProtobuf splitImportText1))

testText :: Test
testText = TestCase $ do
  assertEqual
    "too many semicolons"
    False
    ( isRight
        ( parseProtobuf
            "import \"foo.proto\";;\n\
            \import \"bar.proto\";"
        )
    )
  assertEqual "multiple package" False (isRight (parseProtobuf multiplePackageText))
