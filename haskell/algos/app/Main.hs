module Main where

import GuardsBribe

main :: IO ()
main = do
  putStrLn . show $ leastBribes [1..10]
  putStrLn . show $ lb1 [1,2,3,4, 1,2,3,4, 1,2,3,4]
  putStrLn . show $ leastBribes [1,2,3,4, 1,2,3,4, 1,2,3,4]
  putStrLn . show $ lb1 (map (uncurry (+) . fmap (*53) . (`divMod` 7)) [1..50])
  putStrLn . show $ leastBribes (map (uncurry (+) . fmap (*53) . (`divMod` 7)) [1..50])
--  putStrLn . show $ leastBribes [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
