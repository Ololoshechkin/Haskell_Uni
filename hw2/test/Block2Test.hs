module Block2Test where

import           Block2           (stringSum)
import           Test.Hspec
import           Test.Tasty       (TestTree)
import           Test.Tasty.Hspec (Spec, it, shouldBe, testSpec)

import qualified Hedgehog.Gen     as Gen
import qualified Hedgehog.Range   as Range

testStringSum :: IO TestTree
testStringSum = testSpec "String sum test" main

main  :: Spec
main = it "stringSum" $ do
    stringSum "1 2 3" `shouldBe` Just 6
    stringSum "1 2 -3" `shouldBe` Just 0
    stringSum "akjdw d" `shouldBe` Nothing
