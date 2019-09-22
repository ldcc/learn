import LastDigit (lastDigit)
import Test.Hspec
import Test.QuickCheck

lastDigit' :: [Integer] -> Integer
lastDigit' as = a' where
  (a:b:c:_) = squashZeros as ++ repeat 1
  a' = (a `mod` 10) ^ b'' `mod` 10
  b' = if c==1 then b else (b `mod` 4) ^ c'
  b'' = 1 + (b' - 1) `mod` 4
  c' = 2 + c `mod` 2

squashZeros :: [Integer] -> [Integer]
squashZeros [] = []
squashZeros (0:xs) = [0 | not $ isZero xs]
squashZeros (x:xs) = f x xs where
  f t []     = [t]
  f t (0:zs) = [t | isZero zs]
  f t (z:zs) = t : f z zs

isZero :: [Integer] -> Bool
isZero = foldr (\x acc -> x==0 && not acc) False

examples = do
  it "should work for some examples" $ do
    lastDigit []          `shouldBe` 1
    lastDigit [0,0]       `shouldBe` 1 -- 0 ^ 0
    lastDigit [0,0,0]     `shouldBe` 0 -- 0^(0 ^ 0) = 0^1 = 0
    lastDigit [1,2]       `shouldBe` 1
    lastDigit [12,18]     `shouldBe` 4
    lastDigit [8,21]      `shouldBe` 8
    lastDigit [3,3,1]     `shouldBe` 7
    lastDigit [3,3,2]     `shouldBe` 3
    lastDigit [3,5,3]     `shouldBe` 3
    lastDigit [3,4,5]     `shouldBe` 1
    lastDigit [4,3,6]     `shouldBe` 4
    lastDigit [7,6,1]     `shouldBe` 9
    lastDigit [7,6,2]     `shouldBe` 1
    lastDigit [7,6,21]    `shouldBe` 1
    lastDigit [7,11,2]    `shouldBe` 7
    lastDigit [12,30,21]  `shouldBe` 6
    lastDigit [2,0,1]     `shouldBe` 1
    lastDigit [2,2,2,0]   `shouldBe` 4
    lastDigit [2,2,101,2] `shouldBe` 6
    lastDigit [4,0]       `shouldBe` 1
    lastDigit [3,0,0]     `shouldBe` 3
    lastDigit [2,2,1]     `shouldBe` 4
    lastDigit [2,2,1,2]   `shouldBe` 4
    lastDigit [3,3,0,0]   `shouldBe` 7
    lastDigit [3,4,0]     `shouldBe` 3
    lastDigit [3,2,1,4,4] `shouldBe` 9
    lastDigit [5,0]       `shouldBe` 1 
    lastDigit [2,3,2]     `shouldBe` 2
    lastDigit [0,0,0,0,0,0,0]        `shouldBe` 0
    lastDigit [82242,254719,736371]  `shouldBe` 8
    lastDigit [937640,767456,981242] `shouldBe` 0
    lastDigit [123232,694022,140249] `shouldBe` 6
    lastDigit [499942,898102,846073] `shouldBe` 6
    lastDigit [837142,918895,51096]  `shouldBe` 2
    lastDigit [625703,43898,614961,448629] `shouldBe` 1
    lastDigit [2147483647,2147483647,2147483647,2147483647] `shouldBe` 3

simpleProperties = do
  it "lastDigit [x] == x `mod` 10" $
    property (\(NonNegative x) -> lastDigit [x] == x `mod` 10)
  it "lastDigit [x, y] == x ^ y `mod` 10" $
    property (\(NonNegative x) (NonNegative y) -> lastDigit [x, y] == x ^ y `mod` 10)

normalProperties = do
  it "test lastDigit [x, y, z]" $
    property (\(NonNegative x) (NonNegative y) (NonNegative z) -> lastDigit [x,y,z] == lastDigit' [x,y,z])
  it "test lastDigit [x, y, z, t]" $
    property (\(NonNegative x) (NonNegative y) (NonNegative z) (NonNegative t) -> lastDigit [x,y,z,t] == lastDigit' [x,y,z,t])
  it "test lastDigit [x, y, z, t, u]" $
    property (\(NonNegative x) (NonNegative y) (NonNegative z) (NonNegative t) (NonNegative u) -> lastDigit [x,y,z,t,u] == lastDigit' [x,y,z,t,u])
  it "test lastDigit [x, y, z, t, u, v]" $
    property (\(NonNegative x) (NonNegative y) (NonNegative z) (NonNegative t) (NonNegative u) (NonNegative v) -> lastDigit [x,y,z,t,u,v] == lastDigit' [x,y,z,t,u,v])

propertyN n = property (\(NonNegative x) -> let bits = (map (`mod` n) $ takeWhile (>0) $ iterate (`div` n) x) in lastDigit bits == lastDigit' bits)

longProperties = do
  it "test lists of 0's and 1's" $ propertyN 2
  it "test lists of 0's, 1's and 2's" $ propertyN 3
  it "test lists of 0's, 1's, 2's and 3's" $ propertyN 4
  it "test lists of 0's, 1's, 2's, 3's and 4's" $ propertyN 5
  it "test lists of 0's, 1's, 2's, 3's, 4's and 5's" $ propertyN 6
  it "test lists of 0's, 1's, 2's, 3's, 4's, 5's and 6's" $ propertyN 7
  it "test lists of 0's, 1's, 2's, 3's, 4's, 5's, 6's and 7's" $ propertyN 8
  it "test lists of 0's, 1's, 2's, 3's, 4's, 5's, 6's, 7's and 8's" $ propertyN 9
  it "test lists of 0's, 1's, 2's, 3's, 4's, 5's, 6's, 7's, 8's and 9's" $ propertyN 10

hardProperties = do
  it "are you lazy enough to compute lastDigit [7,7,7,7^7,9^(9^9)]?" $
    lastDigit [7,7,7,7^7,9^(9^9)] `shouldBe` 3

main :: IO ()
main = hspec $ do
  describe "Examples" examples
  describe "Simple properties" simpleProperties
  describe "Normal properties" normalProperties
  describe "Long properties"   longProperties
  describe "Hard properties"   hardProperties