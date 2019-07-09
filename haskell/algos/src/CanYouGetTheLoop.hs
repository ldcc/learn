-- You are given a node that is the beginning of a linked list.
-- This list always contains a tail and a loop.
--
-- Your objective is to determine the length of the loop.
--
-- use the `next :: Node a -> Node a` function to get the following node
-- Note: do NOT mutate the nodes!

module CanYouGetTheLoop where

data Node a = Null | Node a (Node a)

instance Eq a => Eq (Node a) where
  (Node a _) == (Node b _) = a == b
  a /= b = not (a == b)

instance Show a => Show (Node a) where
  show (Node a n) = "Node" ++ show a

next :: Node a -> Node a
next (Node _ n) = n
next _ = Null

loopSize :: Eq a => Node a -> [Node a]
loopSize = loop [] where
  loop ns n
    | n `elem` ns = dropWhile (/= n) $ reverse ns
    | otherwise = loop (n:ns) (next n)

