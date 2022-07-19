module SingletonsSpec where
import Test.Hspec
import Singletons

spec :: Spec
spec = do
  describe "Type test check" $
    it "Type checking" $ 1 `shouldBe` 1