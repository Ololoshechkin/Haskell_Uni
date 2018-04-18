module Block3Test where

import           Block3
import           Data.Char        (isUpper)
import           Test.Tasty       (TestTree)
import           Test.Tasty.Hspec (Spec, it, shouldBe, testSpec)

testParser :: IO TestTree
testParser = testSpec "Parser test" main


main :: Spec
main = do
  it "satisfy" $ do
    runParser (satisfy isUpper) "abc" `shouldBe` Nothing
    runParser (satisfy isUpper) "ABD" `shouldBe` Just ('A',"BD")
  it "ok" $
    runParser ok "xyz" `shouldBe` Just ((),"xyz")
  it "eof" $ do
    runParser eof "" `shouldBe` Just ((),"")
    runParser eof "efdlmc" `shouldBe` Nothing
  it "element" $ do
    runParser (element 'x') "xyz" `shouldBe` Just ('x',"yz")
    runParser (element 'x') "ayz" `shouldBe` Nothing
  it "stream" $ do
    runParser (stream "x") "xyz" `shouldBe` Just ("x","yz")
    runParser (stream "xy") "xyz" `shouldBe` Just ("xy","z")
    runParser (stream "x") "ayz" `shouldBe` Nothing
  it "spaces" $ do
    runParser spaces "   s" `shouldBe` Just ("   ","s")
    runParser spaces "s   s" `shouldBe` Just ("","s   s")
  it "integer" $ do
    runParser integer "12" `shouldBe` Just (12,"")
    runParser integer "dakjdb" `shouldBe` Nothing
  it "balanced" $ do
    runParser balanced "[]" `shouldBe` Just((), "[]")
    runParser balanced "()()()()[]((()()){})" `shouldBe` Just((), "()()()()[]((()()){})")
    runParser balanced "]" `shouldBe` Nothing
  it "onlySignedInteger" $ do
    runParser onlySignedInteger "-12" `shouldBe` Just (-12,"")
    runParser onlySignedInteger "+12" `shouldBe` Just (12,"")
    runParser onlySignedInteger "12" `shouldBe` Nothing
    runParser onlySignedInteger "dakjdb" `shouldBe` Nothing
    runParser onlySignedInteger "-dakjdb" `shouldBe` Nothing
    runParser onlySignedInteger "+dakjdb" `shouldBe` Nothing
  it "notOnlySignedInteger" $ do
    runParser notOnlySignedInteger "-12" `shouldBe` Just (-12,"")
    runParser notOnlySignedInteger "+12" `shouldBe` Just (12,"")
    runParser notOnlySignedInteger "12" `shouldBe` Just (12,"")
    runParser notOnlySignedInteger "dakjdb" `shouldBe` Nothing
    runParser notOnlySignedInteger "-dakjdb" `shouldBe` Nothing
    runParser notOnlySignedInteger "+dakjdb" `shouldBe` Nothing
  it "parseListOfLists" $ do
      runParser parseListOfLists "2, 1,+10  , 3,5,-7, 2" `shouldBe` Just ([ [1, 10], [5, -7, 2] ], "")


