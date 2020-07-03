module Main where

import RegExpParser

main :: IO ()
main = do
  putStrLn . show $ parseRegExp "|*"
