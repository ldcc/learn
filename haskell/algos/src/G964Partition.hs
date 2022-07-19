--https://www.codewars.com/kata/55cf3b567fc0e02b0b00000b/train/haskell
--From wikipedia https://en.wikipedia.org/wiki/Partition_(number_theory)
--
--In number theory and combinatorics, a partition of a positive integer n, also called an integer partition, is a way of writing n as a sum of positive integers. Two sums that differ only in the order of their summands are considered the same partition.
--
--For example, 4 can be partitioned in five distinct ways:
--
--4, 3 + 1, 2 + 2, 2 + 1 + 1, 1 + 1 + 1 + 1.
--
--We can write:
--
--enum(4) -> [[4],[3,1],[2,2],[2,1,1],[1,1,1,1]] and
--
--enum(5) -> [[5],[4,1],[3,2],[3,1,1],[2,2,1],[2,1,1,1],[1,1,1,1,1]].
--
--The number of parts in a partition grows very fast. For n = 50 number of parts is 204226, for 80 it is 15,796,476 It would be too long to tests answers with arrays of such size. So our task is the following:
--
--1 - n being given (n integer, 1 <= n <= 50) calculate enum(n) ie the partition of n. We will obtain something like that:
--enum(n) -> [[n],[n-1,1],[n-2,2],...,[1,1,...,1]] (order of array and sub-arrays doesn't matter). This part is not tested.
--
--2 - For each sub-array of enum(n) calculate its product. If n = 5 we'll obtain after removing duplicates and sorting:
--
--prod(5) -> [1,2,3,4,5,6]
--
--prod(8) -> [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 15, 16, 18]
--
--If n = 40 prod(n) has a length of 2699 hence the tests will not verify such arrays. Instead our task number 3 is:
--
--3 - return the range, the average and the median of prod(n) in the following form (example for n = 5):
--
--"Range: 5 Average: 3.50 Median: 3.50"
--
--Range is an integer, Average and Median are float numbers rounded to two decimal places (".2f" in some languages).
--
--Notes:
--Range : difference between the highest and lowest values.
--
--Mean or Average : To calculate mean, add together all of the numbers in a set and then divide the sum by the total count of numbers.
--
--Median : The median is the number separating the higher half of a data sample from the lower half. (https://en.wikipedia.org/wiki/Median)

module G964Partition where
import Text.Printf (printf)
import Data.List (sort, group)
import Data.Function (on)
import Data.Maybe (fromJust)
import Data.Map as M (Map, fromList, insert, lookup)

part :: Int -> String
part = range . enum

isPrime :: Int -> Bool
isPrime n = all (\ d -> n `mod` d /= 0) [2 .. floor . sqrt . fromIntegral $ n]

enum :: Int -> [Int]
enum n = fromJust $ M.lookup n (_enum (fromList []) n)
  where
    _lookup pmap n = case M.lookup n pmap of
      Just v -> (v, pmap)
      Nothing -> let nmap = _enum pmap n in (fromJust $ M.lookup n nmap, nmap)
    merge = (.) (map head . group . sort) . (++)
    _enum pmap 1 = fromList [(1, [1])]
    _enum pmap n = foldl f (insert n [n] pmap) $ filter isPrime [1 .. div n 2]
      where
        f _pmap k = insert n (merge (fromJust $ M.lookup n _pmap) ((*) <$> e1 <*> e2)) (insert (n-k) e2 m2)
          where
            (e1, m1) = _lookup _pmap k
            (e2, m2) = _lookup (insert k e1 m1) (n-k)

range :: [Int] -> String
range ps = printf "Range: %d Average: %.2f Median: %.2f" (rng ps) (avg ps) (med ps)
  where
    rng = (-) <$> last <*> head
    avg :: [Int] -> Float
    avg = on (/) fromIntegral <$> sum <*> length
    med :: [Int] -> Float
    med ps
      | odd n = fromIntegral $ ps !! div n 2
      | even n = ((.) (/2.0) . on (+) med <$> tail <*> init) ps
      where n = length $ ps
