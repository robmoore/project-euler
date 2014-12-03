-- 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
--
-- What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
module Prob5 where

-- Note: Could have used built-in gcd and lcd functions

-- Due to http://math.cmu.edu/~bkell/21110-2010s/extended-euclidean.html:
gcd' :: Integral a => a -> a -> a
gcd' x y
   | r == 0 = d
   | otherwise = gcd' d r
   where c = max x y
         d = min x y
         r = mod c d

lcm' :: Integral a => a -> a -> a
lcm' x y = div (x * y) $ gcd' x y

mlcm :: Integral a => [a] -> a
mlcm (x:xs) = foldr lcm' x xs

main :: IO ()
main = print $ mlcm [1..20]


