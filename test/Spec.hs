import Test.HUnit (Test (TestList), runTestTTAndExit)
import Unit.Enum as Unit (allTests)
import Unit.Misc as Misc (allTests)

main :: IO ()
main =
  runTestTTAndExit
    ( TestList
        ( Unit.allTests ++ Misc.allTests
        )
    )
