module GuardsBribe where

import Data.Array
data Trie = Node Int Trie Trie | Null

instance Show Trie where
  show = show . bribe
--  show (Node fp Null Null) = "Node " ++ show fp ++ ""
--  show (Node fp l Null) = "Node " ++ show fp ++ " (" ++ show l ++ ")"
--  show (Node fp Null r) = "Node " ++ show fp ++ " (" ++ show r ++ ")"
--  show (Node fp l r) = "Node " ++ show fp ++ " (" ++ show l ++ ", " ++ show r ++ ")"
--  show (Null) = ""

instance Eq Trie where
  a == b = bribe a == bribe b

instance Ord Trie where
  a < b = bribe a < bribe b
  a <= b = bribe a <= bribe b
  a > b = bribe a > bribe b
  a >= b = bribe a >= bribe b

leastBribes :: [Int] -> Int
leastBribes = bribe . genTrie

genTrie :: [Int] -> Trie
genTrie xs = case findFP >>= splitAt $ xs of
  ([], []) -> Null
  ([x], []) -> Node x Null Null
--  ((x:[y]), []) -> Node x (Node y Null Null) Null
  (l, r) -> min t1 t2
    where
      t1 = Node (last l) (genTrie $ take (length l - 1) l) (genTrie r)
      t2 = Node (head r) (genTrie l) (genTrie $ tail r)

-- find the indices for `fixed point` in a list which weight of balance with amount of bribes
-- for every `fixed point` will be an  exact binary node.
findFP :: [Int] -> Int
findFP xs
  | length xs < 2 = length xs
  | otherwise = findfp [head xs] [last xs] (drop 1 . take (length xs - 1) $ xs)
  where
    findfp l r xs
      | length xs < 2 = 1 + length xs
      | v1 == v3 && v4 < v1 && v3 < v2 = 1 + findfp (l ++ l1) r tl
      | v2 == v4 && v4 < v1 && v3 < v2 = findfp l (r1 ++ r) tr
      | v1 < v3 = 1 + findfp (l ++ l1) r tl
      | otherwise = findfp l (r1 ++ r) tr
      where
        (l1, tl) = splitAt 1 xs
        (tr, r1) = splitAt (length xs - 1) xs
        v1 = genTrie l
        v2 = genTrie (l ++ l1)
        v3 = genTrie r
        v4 = genTrie (r1 ++ r)

-- determine the minimum amount in the worst case
bribe :: Trie -> Int
bribe (Null) = 0
bribe (Node k l r) = k + max (bribe l) (bribe r)




lb1 :: [Int] -> Int
lb1 pays = let
    n = length pays
    a = array ((1,1),(n,n)) $
        [((1, i), pays!!(i-1)) | i <- [1..n]] ++
        [((2, i), (zipWith (+) pays (tail pays))!!(i-1)) | i <- [1..(n-1)]] ++
        [((i, j), minimum [(pays!!(k-1)) + (max (a!(k-j, j)) (a!(i+j-k-1, k+1))) | k <- [(j+1)..(j+i-2)]])
        | i <- [3..n], j <- [1..(n-i+1)]]
  in  a!(n,1)