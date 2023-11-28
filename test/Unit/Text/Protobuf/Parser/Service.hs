module Unit.Text.Protobuf.Parser.Service (allTests) where

import Data.Either (fromRight, isRight)
import Test.HUnit
import Text.Parsec (parse)
import Text.Protobuf.Parser.Service
import Text.Protobuf.Types

allTests :: [Test]
allTests =
  [ TestLabel "simple" testSimple
  ]

failMessage :: Service
failMessage = Service "FAIL" []

simpleServiceText :: String
simpleServiceText =
  "service SearchService {\n\
  \  rpc Search(SearchRequest) returns (SearchResponse) {}\n\
  \}"

simpleService :: Service
simpleService =
  Service
    "SearchService"
    [ RPC
        "Search"
        (RequestType "SearchRequest")
        (ReplyType "SearchResponse")
    ]

multipleServiceText :: String
multipleServiceText =
  "service Multiple {\n\
  \  rpc Search(Foo) returns (Bar) {}\n\
  \  rpc Search1(Bar) returns (Foo) {}\n\
  \}"

multipleService :: Service
multipleService =
  Service
    "Multiple"
    [ RPC
        "Search"
        (RequestType "Foo")
        (ReplyType "Bar"),
      RPC
        "Search1"
        (RequestType "Bar")
        (ReplyType "Foo")
    ]

streamRequestServiceText :: String
streamRequestServiceText =
  "service Multiple {\n\
  \  rpc Search(stream Foo) returns (Bar) {}\n\
  \}"

streamRequestService :: Service
streamRequestService =
  Service
    "Multiple"
    [ RPC
        "Search"
        (RequestTypeStream "Foo")
        (ReplyType "Bar")
    ]

streamReplyServiceText :: String
streamReplyServiceText =
  "service Multiple {\n\
  \  rpc Search(Foo) returns (stream Bar) {}\n\
  \}"

streamReplyService :: Service
streamReplyService =
  Service
    "Multiple"
    [ RPC
        "Search"
        (RequestType "Foo")
        (ReplyTypeStream "Bar")
    ]

testSimple :: Test
testSimple = TestCase $ do
  assertEqual
    "empty"
    False
    (isRight (parse parseService "" ""))
  assertEqual
    "keyword only"
    False
    (isRight (parse parseService "" "message"))
  assertEqual
    "missing name"
    False
    (isRight (parse parseService "" "message {}"))
  assertEqual
    "emptyMessage"
    simpleService
    (fromRight failMessage (parse parseService "" simpleServiceText))
  assertEqual
    "multiple"
    multipleService
    (fromRight failMessage (parse parseService "" multipleServiceText))
  assertEqual
    "stream request"
    streamRequestService
    (fromRight failMessage (parse parseService "" streamRequestServiceText))
  assertEqual
    "stream reply"
    streamReplyService
    (fromRight failMessage (parse parseService "" streamReplyServiceText))
