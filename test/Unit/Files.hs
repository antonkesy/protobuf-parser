module Unit.Files (allTests) where

import Data.Either (fromRight)
import ProtoParser
import Protobuf
import Test.HUnit

allTests :: [Test]
allTests =
  [TestLabel "fileTest" testFiles]

getResult :: FilePath -> IO Protobuf
getResult fileNameWithoutExtension = do
  fromRight emptyProtobuf <$> parseProtoFile ("test/protofiles/" ++ fileNameWithoutExtension ++ ".proto")

assertProtoFile :: FilePath -> Protobuf -> Assertion
assertProtoFile fileNameWithoutExtension expected = do
  p <- getResult fileNameWithoutExtension
  assertEqual fileNameWithoutExtension expected p

testFiles :: Test
testFiles = TestCase $ do
  assertProtoFile
    "1"
    ( Protobuf
        { syntax = Nothing,
          package = Nothing,
          imports = ["foo.proto"],
          options = [],
          enums = [],
          messages =
            [ Message
                "SearchRequest"
                [ ImplicitMessageField (Scalar (IntType Int32)) "page_number" 2,
                  ImplicitMessageField (Scalar (FloatType Double)) "results_per_page" 3
                ]
            ],
          services = []
        }
    )
  assertProtoFile
    "2"
    ( Protobuf
        { syntax = Nothing,
          package = Just "foobar",
          imports = ["foo.proto", "bar.proto"],
          options = [],
          enums = [],
          messages =
            [ Message
                "SearchRequest"
                [ ImplicitMessageField (Scalar (IntType Int32)) "page_number" 2,
                  ImplicitMessageField (Scalar (FloatType Double)) "results_per_page" 3
                ],
              Message
                "SearchResponse"
                [ ImplicitMessageField (Scalar StringType) "name" 1
                ]
            ],
          services = []
        }
    )
