module Main where

import RecoverSecretFromTriplets

-- whatisup
-- mathisfun
triplets = ["tup"
                      ,"whi"
                      ,"tsu"
                      ,"ats"
                      ,"hap"
                      ,"tis"
                      ,"whs"
                      ]


main :: IO ()
main = putStrLn . show $ recoverSecret triplets