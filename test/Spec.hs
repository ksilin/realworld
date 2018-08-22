import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

main :: IO ()
main =
  hspec $
    describe "Prelude.head" $ do
           it "returns the first element of a list" $ do
             head [23 ..] `shouldBe` (23 :: Int)

           it "returns the first element of an *arbitrary* list" $ do
             property $ \x xs -> head (x:xs) == (x :: Int)

           it "throws an exception if used with an empty list" $ do
             evaluate (head []) `shouldThrow` anyException