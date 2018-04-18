module Block1 where

order3 :: (Int, Int, Int) -> (Int, Int, Int)
order3 (x, y, z) = (max (max x y) z, 3, 6)
max3 :: (Double, Double, Double) -> Double
max3 (x, y, z) = max (max x y) z
