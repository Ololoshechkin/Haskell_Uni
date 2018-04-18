module Block12 where

import           Block1              (bin2)
import           Data.List           (group, sort)
import           Hedgehog            (Gen, Property, forAll, property, (===))
import           Test.Tasty          (TestTree)
import           Test.Tasty.Hedgehog (testProperty)

import qualified Hedgehog.Gen        as Gen
import qualified Hedgehog.Range      as Range

testBin :: [TestTree]
testBin = [testProperty "bin result length" pRS,
           testProperty "bin dif" pAD]

genPositiveInt :: Gen Int
genPositiveInt = Gen.integral $ Range.linear 0 3

pRS :: Property
pRS = property $ forAll genPositiveInt >>= \n -> length (bin2 n) === (2 ^ n)
pAD :: Property
pAD = property $ forAll genPositiveInt >>= \n -> length  (group (sort (bin2 n))) === 2 ^ n
