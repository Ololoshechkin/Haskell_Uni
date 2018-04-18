module Block11 where

import           Block1
import           Block2           (stringSum)
import           Test.Hspec
import           Test.Tasty       (TestTree)
import           Test.Tasty.Hspec (Spec, describe, it, shouldBe, testSpec)

testEval :: IO TestTree
testEval = testSpec "Eval test" main


main  :: Spec
main = do
  describe "expression" $ do
    it "const " $ eval (Const 6) `shouldBe` Right 6
    it "NegativePower " $ eval (Pow (Const 0) (Const (-2))) `shouldBe` Left NegativePower
    it "DivisionByZero " $ eval (Div (Const 1) (Const 0)) `shouldBe` Left DivisionByZero
    it "Add " $ eval (Add (Const 1) (Const 2)) `shouldBe` Right 3
    it "Mul " $ eval (Mul (Const 3) (Const 2)) `shouldBe` Right 6
    it "Sub " $ eval (Sub (Const 5) (Const 2)) `shouldBe` Right 3
    it "Div " $ eval (Div (Const 6) (Const 2)) `shouldBe` Right 3
  it "bin" $ do
    bin2 1 `shouldMatchList` [[1], [0]]
    bin2 2 `shouldMatchList` [[1, 0], [0, 1], [1, 1], [0, 0]]
  it "stringSum" $ do
    stringSum "1 2 3" `shouldBe` Just 6
    stringSum "1 2 -3" `shouldBe` Just 0
    stringSum "akjdw d" `shouldBe` Nothing


