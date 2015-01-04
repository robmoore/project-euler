-- The prime factors of 13195 are 5, 7, 13 and 29.
-- What is the largest prime factor of the number 600851475143 ?

module Prob3 where

import           Data.Numbers.Primes

-- http://www.ehow.com/how_8136601_largest-prime-factor-composite-number.html
lpfc :: Integral a => a -> [a] -> [a]
lpfc i p
  | isPrime i = [i] --  base case
  | mod i hp == 0 = hp : lpfc (div i hp) primes -- evenly divisible by a prime; add divisor to list and analyze dividend using fresh primes
  | otherwise = lpfc i $ tail p -- try with next prime
  where hp = head p

main :: IO ()
main = do
    let solution = last $ lpfc 600851475143 primes
    print solution
