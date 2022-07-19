-- {-# OPTIONS_GHC -F -pgmF hspec-discover #-}

import Test.Hspec
import qualified SimpleSQLEngineSpec
import qualified RegExpParserSpec
import qualified UnlimitedGameOfLifeSpec
import qualified SingletonsSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Singletons" SingletonsSpec.spec
--  describe "SQLEngine" SimpleSQLEngineSpec.spec
--  describe "RegExpParser" RegExpParserSpec.spec
--  describe "GameOfLife" UnlimitedGameOfLifeSpec.spec