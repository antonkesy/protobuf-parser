import Test.HUnit (Test (TestList), runTestTTAndExit)
import Unit.Enum as Unit
import Unit.Import as Import
import Unit.Misc as Misc
import Unit.ProtoParser as Protobuf
import Unit.Comment as Comment

main :: IO ()
main =
  runTestTTAndExit
    ( TestList
        ( Unit.allTests
            ++ Misc.allTests
            ++ Import.allTests
            ++ Protobuf.allTests
            ++ Comment.allTests
        )
    )
