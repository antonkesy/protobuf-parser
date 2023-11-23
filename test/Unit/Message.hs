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

testSimple :: Test
testSimple = TestCase $ do
  assertEqual "empty" False (isRight (parse parseMessage "" ""))
  assertEqual "keyword only" False (isRight (parse parseMessage "" "message"))
  assertEqual "missing name" False (isRight (parse parseMessage "" "message {}"))
  assertEqual "emptyMessage" (Message "Foo" []) (fromRight failMessage (parse parseMessage "" "message Foo {}"))

----------------------------------------------------------------

defaulTestMap :: MessageField
defaulTestMap = MessageField (Map (StringKey "") (MapName "")) "TEST" 0 False

-- map<key_type, value_type> map_field = N;
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
