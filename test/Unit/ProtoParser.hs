module Unit.ProtoParser (allTests) where

import Data.Either (fromRight, isRight)
import ProtoParser
import Protobuf
import Test.HUnit

allTests :: [Test]
allTests =
  [ TestLabel "text" testText,
    TestLabel "splittedDefinitions" testSplittedDefinitions,
    TestLabel "comments" testComments
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
      { package = Just "foobar",
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

textComment :: Protobuf
textComment =
  ( Protobuf
      { package = Just "foobar",
        imports = ["foo.proto", "bar.proto"],
        options = [],
        enums = [],
        messages = [],
        services = []
      }
  )

testComment1 :: String
testComment1 =
  "import \"foo.proto\";\n\
  \// comment\n\
  \package foobar;\n\
  \import \"bar.proto\";"

testComment2 :: String
testComment2 =
  "import \"foo.proto\";\n\
  \/* comment */\n\
  \package foobar;\n\
  \import \"bar.proto\";"

testComment3 :: String
testComment3 =
  "import \"foo.proto\";\n\
  \package /* comment */ foobar;\n\
  \import \"bar.proto\";"

testComment4 :: String
testComment4 =
  "import \"foo.proto\";\n\
  \package /* comment \n\n */ foobar;\n\
  \import \"bar.proto\";"

testComments :: Test
testComments = TestCase $ do
  assertEqual "whole line 1" textComment (fromRight defaultTestProto (parseProtobuf testComment1))
  assertEqual "whole line 1" textComment (fromRight defaultTestProto (parseProtobuf testComment2))
  assertEqual "in-line" textComment (fromRight defaultTestProto (parseProtobuf testComment3))
  assertEqual "multi in-line" textComment (fromRight defaultTestProto (parseProtobuf testComment4))
