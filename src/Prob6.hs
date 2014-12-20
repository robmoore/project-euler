-- The sum of the squares of the first ten natural numbers is -- 12 + 22 + ... + 102 = 385
--
-- The square of the sum of the first ten natural numbers is, -- (1 + 2 + ... + 10)2 = 552 = 3025
--
-- Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is 3025 − 385 = 2640.
--
-- Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.

module Prob6 where

squareOfSums :: Num a => [a] -> a
squareOfSums xs = sum xs ^ 2

sumOfSquares :: Num a => [a] -> a
sumOfSquares xs = sum $ map (^2) xs

main :: IO()
main = print $ squareOfSums [1..100] - sumOfSquares [1..100]