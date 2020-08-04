-- {-# OPTIONS_GHC -F -pgmF hspec-discover #-}

import Test.Hspec
import qualified SimpleSQLEngineSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "SQLEngine" SimpleSQLEngineSpec.spec