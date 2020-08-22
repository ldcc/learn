module PatAge where

patage :: (Integral a, Fractional a) => String -> (a, String)
patage birth
  | y >= 1 = (y, "岁")
  | nowm /= birm = (mod (nowm - birm) 12, "月")
  | otherwise = (((unix now) - (unix birth)) / 60 / 60 / 24, "天")
  where
    y = year(now) - year(birth)
    nowm = month(now)
    birm = month(birth)

year :: (Integral a, Fractional a) =>  String -> a
year birth = 2020
month :: (Integral a, Fractional a) => String -> a
month birth = 8
unix :: (Integral a, Fractional a) => String -> a
unix birth = 0


now :: String
now = ""