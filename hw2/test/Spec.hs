import Test.Tasty (defaultMain, testGroup)
import Block1Test (testEval, testBin)
import Block2Test (testStringSum)
import Block3Test (testParser)
import Control.Applicative

main :: IO ()
main = liftA2 (\x y-> [x, y]) testEval testParser >>=
    \test ->  testStringSum >>=
        \testString -> defaultMain $ testGroup "hw2" $ testString : testBin ++ test