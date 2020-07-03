module Main where

import RegExpParser

main :: IO ()
main = do
  putStrLn . show $ pickExp 0 "*"
  putStrLn . show $ parseRegExp "*"
