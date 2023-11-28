import E2E.Files as Files
import Test.HUnit (Test (TestList), runTestTTAndExit)
import Unit.Text.Protobuf.Parser as Protobuf
import Unit.Text.Protobuf.Parser.Comment as Comment
import Unit.Text.Protobuf.Parser.Enum as Unit
import Unit.Text.Protobuf.Parser.Import as Import
import Unit.Text.Protobuf.Parser.LexicalElement.Constant as Constant
import Unit.Text.Protobuf.Parser.Message as Message
import Unit.Text.Protobuf.Parser.Option as Option
import Unit.Text.Protobuf.Parser.Package as Package
import Unit.Text.Protobuf.Parser.Service as Service
import Unit.Text.Protobuf.Parser.Syntax as Syntax
import Unit.Text.Protobuf.Parser.Type as Type

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
            ++ Syntax.allTests
            ++ Option.allTests
            ++ Constant.allTests
        )
    )
