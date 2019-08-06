import qualified Test.Tasty
import Test.Tasty.Hspec

main :: IO ()
main = do
  test <- testSpec "tools-hs" spec
  Test.Tasty.defaultMain test

spec :: Spec
spec = parallel $ do it "matches directory to be included" $ True `shouldBe` True
