module JunkSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck
import Control.Monad

import Data.String.Strip

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "int number" $ do
    when False $ do
      let prop_should_be_failure x = (x <= 0) ==> False
            where types = x :: Int
      it "should be failure(1)" $ property prop_should_be_failure

    when False $ do
      let prop_should_be_failure2 x = x < 30
            where types  = x :: Int
      modifyMaxSuccess (const 1) $
        it "should be failure(2)" $ (property prop_should_be_failure2)