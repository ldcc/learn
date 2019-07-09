module MultNumAsStrings where

import Data.Char (intToDigit, digitToInt)

multiply :: String -> String -> String
multiply xs ys = show $ read xs * read ys
