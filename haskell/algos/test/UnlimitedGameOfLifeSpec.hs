module UnlimitedGameOfLifeSpec (spec) where
import UnlimitedGameOfLife (getGeneration)
import Test.Hspec
import Test.HUnit
import Data.List (intercalate)

gliders = [[[1,0,0],
            [0,1,1],
            [1,1,0]],
           [[0,1,0],
            [0,0,1],
            [1,1,1]],
           [[1,0,1],
            [0,1,1],
            [0,1,0]],
           [[0,0,1],
            [1,0,1],
            [0,1,1]]
          ]

twoGliders = [[[1,1,1,0,0,0,1,0],
               [1,0,0,0,0,0,0,1],
               [0,1,0,0,0,1,1,1]],
              [[1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1]]
             ]

fPentomino = [[0, 1, 0], [0, 1, 1], [1, 1, 0]]

spec :: Spec
spec = do
  describe ("Glider\n" ++ htmlize (gliders !! 0)) $ do
    it "Glider 0" $ do
      assertLife (gliders !! 0) 0 (gliders !! 0)
    it "Glider 1" $ do
      assertLife (gliders !! 0) 1 (gliders !! 1)
    it "Glider 2" $ do
      assertLife (gliders !! 0) 2 (gliders !! 2)
    it "Glider 3" $ do
      assertLife (gliders !! 0) 3 (gliders !! 3)
    it "Glider 40" $ do
      assertLife (gliders !! 0) 40 (gliders !! 0)

  describe ("Two Gliders\n" ++ htmlize (twoGliders !! 0)) $ do
    it "Two Gliders 100" $ do
      assertLife (twoGliders !! 0) 16 (twoGliders !! 1)

assertLife start gen expected = do
    let actual = getGeneration start gen
        errorMsg = intercalate "\n" ["expected:", htmlize expected, "got:" , htmlize actual]

    assertBool errorMsg (actual == expected)


htmlize :: [[Int]] -> String
htmlize = concatMap ((++ "\n") . concatMap cell)
  where cell n = if n > 0 then "#" else "0"
