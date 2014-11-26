-- A palindromic number reads the same both ways. The largest palindrome made from the product of
-- two 2-digit numbers is 9009 = 91 × 99.
--
-- Find the largest palindrome made from the product of two 3-digit numbers.

module Prob4 where

import qualified Data.List as DL (filter)

-- We only need the first value so really a lazy list would be preferable. However, the result
-- from map is not sorted (a high fst value and a low snd value results in a palindrome may be
-- less than what will be returned subsequently in the next fst value (998, say) and a higher
-- snd value (that is, the sum of the later multicand and the multiplier are greater than the
-- former value). Hence, this implementation must sort the results to determine a solution.
largestPalindrome :: Int -> Int
largestPalindrome n = maximum pal
    where range = reverse [1..(read $ replicate n '9')]
          prd = map (\x -> head x * last x) $ combinations 2 range
          pal = DL.filter ((\y -> fst y == reverse (snd y)) . splitAt n . show) prd

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x:xs) = map (x:) (combinations (n - 1) xs) ++ combinations n xs

main :: IO ()
main = do
    let p = largestPalindrome 3
    print p
