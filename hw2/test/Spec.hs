import Test.Tasty (defaultMain, testGroup)
import Block11 (testEval)
import Block12 (testBin)
import Block3Test (testParser)
import Control.Applicative

main :: IO ()
main = liftA2 (\x y-> [x, y]) testEval testParser >>= (\test -> defaultMain $ testGroup "hw2" $ testBin ++ test)