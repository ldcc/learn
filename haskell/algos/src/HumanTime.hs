-- Write a function, which takes a non-negative integer (seconds) as input
-- and returns the time in a human-readable format (HH:MM:SS)
--
-- HH = hours, padded to 2 digits, range: 00 - 99
-- MM = minutes, padded to 2 digits, range: 00 - 59
-- SS = seconds, padded to 2 digits, range: 00 - 59
-- The maximum time never exceeds 359999 (99:59:59)

module HumanTime where

humanReadable :: Int -> String
humanReadable x
  | x >= 359999 = "99:59:59"
  | otherwise = show' h ++ ":" ++ show' m ++ ":" ++ show' s
  where
    (h, y) = x `divMod` 3600
    (m, s) = y `divMod` 60

show' :: Int -> String
show' x
  | x < 10 = '0' : show x
  | otherwise = show x