module Unit.Message (allTests) where

import Data.Either (fromRight, isRight)
import ProtoParser.Message
import Protobuf
import Test.HUnit
import Text.Parsec (parse)

allTests :: [Test]
allTests =
  [ TestLabel "simple" testSimple,
    TestLabel "map" testMap
  ]

failMessage :: Message
failMessage = Message "FAIL" []

testMessage1 :: String
testMessage1 =
  "message Foo {\
  \int32 foo = 1;\
  \double bar = 2;\
  \}"

testMessage1Proto :: Message
testMessage1Proto =
  Message
    "Foo"
    [ MessageField (Scalar (IntType Int32)) "foo" 1 False,
      MessageField (Scalar (FloatType Double)) "bar" 2 False
    ]

testSimple :: Test
testSimple = TestCase $ do
  assertEqual "empty" False (isRight (parse parseMessage "" ""))
  assertEqual "keyword only" False (isRight (parse parseMessage "" "message"))
  assertEqual "missing name" False (isRight (parse parseMessage "" "message {}"))
  assertEqual "emptyMessage" (Message "Foo" []) (fromRight failMessage (parse parseMessage "" "message Foo {}"))
  assertEqual "simple" testMessage1Proto (fromRight failMessage (parse parseMessage "" testMessage1))

----------------------------------------------------------------

defaulTestMap :: MessageField
defaulTestMap = MessageField (Map (StringKey "") (MapName "")) "TEST" 0 False

testMap :: Test
testMap = TestCase $ do
  assertEqual "empty" False (isRight (parse parseMap "" ""))
  assertEqual "keyword only" False (isRight (parse parseMap "" "map"))
  assertEqual
    "Simple"
    ( MessageField (Map (StringKey "T") (MapName "V")) "name" 2 False
    )
    (fromRight defaulTestMap (parse parseMap "" "map<T,V> name = 2"))
  assertEqual
    "Simple"
    ( MessageField (Map (IntKey Int32) (MapName "V")) "name" 2 False
    )
    (fromRight defaulTestMap (parse parseMap "" "map<int32,V> name = 2"))
