module ProductFib where

productFib :: Integer -> (Integer, Integer, Bool)
productFib = fibs 0 1
  where fibs a b c = if a * b >= c then (a, b, a * b == c) else fibs b (a + b) c