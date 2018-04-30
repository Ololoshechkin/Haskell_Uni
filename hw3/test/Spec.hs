import Test.Tasty (defaultMain, testGroup)
import InterpreterTest (testEval)
import Control.Applicative

main :: IO ()
main =  testEval  >>=
    \test -> defaultMain $ testGroup "hw3" $ [test]