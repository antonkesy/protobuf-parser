import Test.HUnit (Test (TestList), runTestTTAndExit)
import Unit.Comment as Comment
import Unit.Enum as Unit
import Unit.Files as Files
import Unit.Import as Import
import Unit.Message as Message
import Unit.Package as Package
import Unit.ProtoParser as Protobuf
import Unit.Service as Service
import Unit.Type as Type

main :: IO ()
main =
  runTestTTAndExit
    ( TestList
        ( Unit.allTests
            ++ Type.allTests
            ++ Import.allTests
            ++ Protobuf.allTests
            ++ Comment.allTests
            ++ Message.allTests
            ++ Package.allTests
            ++ Service.allTests
            ++ Files.allTests
        )
    )
