module Data.String.StripSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck

import Data.String.Strip

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "strip" $ do
    it "removes leading and trailing whitespace" $ do
      strip "\t  foo bar\n" `shouldBe` "foo bar"
    it "is idempotent" $ property $
      \str -> strip str === strip (strip str)

  describe "int number" $ do
    let prop_should_be_failure x = (x <= 0) ==> False
          where types = x :: Int
    it "should be failure(1)" $ property prop_should_be_failure

    let prop_should_be_failure2 x = x < 30
          where types  = x :: Int
    modifyMaxSuccess (const 1) $
      it "should be failure(2)" $ (property prop_should_be_failure2)