module Main where


import RegExpParser

main :: IO ()
main = do
  putStrLn . show $ parseRegExp ""
  putStrLn . show $ parseRegExp "ab*"
  putStrLn . show $ parseRegExp "(ab)*"
  putStrLn . show $ parseRegExp "ab|a"
  putStrLn . show $ parseRegExp "a(b|a)"
  putStrLn . show $ parseRegExp "a|b*"
  putStrLn . show $ parseRegExp "(a|b)*"
  putStrLn . show $ parseRegExp "((aa)|ab)*|a"
  putStrLn . show $ parseRegExp "((a.)|.b)*|a"
