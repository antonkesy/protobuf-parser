module Unit.Import (allTests) where

import Data.Either (fromRight, isRight)
import ProtoParser.Import
import Protobuf (ImportPath)
import Test.HUnit
import Text.Parsec (parse)

allTests :: [Test]
allTests =
  [ TestLabel "import" testImport
  ]

testDefault :: ImportPath
testDefault = "TestDefault"

simplePath :: ImportPath
simplePath = "path.proto"

complexPath :: ImportPath
complexPath = "google/protobuf/descriptor.proto"

testImport :: Test
testImport = TestCase $ do
  assertEqual "empty" False (isRight (parse parseImport "" ""))
  assertEqual "missing path" False (isRight (parse parseImport "" "import"))
  assertEqual "missing 'proto;'" False (isRight (parse parseImport "" "import \"path\""))
  assertEqual "missing 'proto'" False (isRight (parse parseImport "" "import \"path\";"))
  assertEqual "missing ';'" False (isRight (parse parseImport "" "import \"path.proto\""))
  assertEqual "missing proto" simplePath (fromRight testDefault (parse parseImport "" ("import \"" ++ simplePath ++ "\";")))
  assertEqual "simple path" simplePath (fromRight testDefault (parse parseImport "" ("import \"" ++ simplePath ++ "\";")))
  assertEqual "complex path" complexPath (fromRight testDefault (parse parseImport "" ("import \"" ++ complexPath ++ "\";")))
