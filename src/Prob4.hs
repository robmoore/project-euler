-- A palindromic number reads the same both ways. The largest palindrome made from the product of
-- two 2-digit numbers is 9009 = 91 × 99.
--
-- Find the largest palindrome made from the product of two 3-digit numbers.

module Prob4 where

import qualified Data.List as DL (minimumBy, nub)

-- Not the most efficient means to do this since we produce duplicate pairs. Also, we only need the first
-- value so really a lazy list would be preferable. See combine function in challenge-8.hs.
largestPalindrome :: Int -> Int
largestPalindrome n = DL.minimumBy (flip compare) p
    where r = reverse [1..(read $ replicate n '9')]
          p = DL.nub [prod | i <- r, j <- r, let prod = i * j, let s = splitAt n $ show prod, fst s == reverse (snd s)]

main :: IO ()
main = do
    let p = largestPalindrome 3
    print p
