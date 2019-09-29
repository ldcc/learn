module Main where

import PeteBake

main :: IO ()
main = do
  putStrLn . show $ cakes [("flour",500), ("sugar",200), ("eggs",1)] [("flour",1200), ("sugar",1200), ("eggs",5), ("milk",200)]