module Main where

import RegExpParser

main :: IO ()
main = do
  putStrLn . show $ parseRegExp "a*a"
  putStrLn . show $ parseRegExp "(a)*a"
  putStrLn . show $ parseRegExp "*a"
  putStrLn . show $ parseRegExp "*aa"
  putStrLn . show $ parseRegExp "**a"
