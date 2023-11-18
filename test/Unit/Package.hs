module Unit.Package (allTests) where

import Data.Either (fromRight, isRight)
import ProtoParser.Package
import Protobuf (Package)
import Test.HUnit
import Text.Parsec (parse)

allTests :: [Test]
allTests =
  [ TestLabel "package" testPackage
  ]

testPackage :: Test
testPackage = TestCase $ do
  assertEqual "empty" False (isRight (parse parsePackage "" ""))
  assertEqual "missing package name" False (isRight (parse parsePackage "" "package"))
  assertEqual "missing ';'" False (isRight (parse parsePackage "" "package foo.bar"))
  assertEqual "Simple" "foo" (fromRight "incorrect" (parse parsePackage "" "package foo;"))
  assertEqual "Complex" "foo.bar" (fromRight "incorrect" (parse parsePackage "" "package foo.bar;"))
