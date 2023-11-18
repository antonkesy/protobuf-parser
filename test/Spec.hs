import Test.HUnit (Test (TestList), runTestTTAndExit)
import Unit.Comment as Comment
import Unit.Enum as Unit
import Unit.Import as Import
import Unit.Message as Message
import Unit.Misc as Misc
import Unit.Package as Package
import Unit.ProtoParser as Protobuf

main :: IO ()
main =
  runTestTTAndExit
    ( TestList
        ( Unit.allTests
            ++ Misc.allTests
            ++ Import.allTests
            ++ Protobuf.allTests
            ++ Comment.allTests
            ++ Message.allTests
            ++ Package.allTests
        )
    )
