module Unit.Text.Protobuf.Parser.LexicalElement.Constant (allTests) where

import Data.Either (fromRight, isRight)
import Test.HUnit
import Text.Parsec (parse)
import Text.Protobuf.Parser.LexicalElement.Constant
import Text.Protobuf.Types

allTests :: [Test]
allTests =
  [ TestLabel "fullIdent" testFullIdent
  ]

testDefault :: Constant
testDefault = ConstantStrLit "FAIL"

testFullIdent :: Test
testFullIdent = TestCase $ do
  assertEqual "empty" False (isRight (parse constant "" ""))
  assertEqual "bool" (ConstantBoolLit True) (fromRight testDefault (parse constant "" "true"))
  assertEqual "bool" (ConstantFullIdent "a.b") (fromRight testDefault (parse constant "" "a.b"))
