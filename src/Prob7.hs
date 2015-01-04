-- By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
-- What is the 10001st prime number?

-- Sieve of Eratosthenes:
-- Make a list of all the integers less than or equal to n (greater than one) and strike out the multiples of all
-- primes less than or equal to the square root of n, then the numbers that are left are the primes.

module Prob7 where

primes :: Int -> [Int]
primes n = take n $ 2 : filter isPrime odds
   where estP = pOfN (fromIntegral n)
         sqrtOfEstP = floor $ sqrt estP
         odds = [3,5..]
         isMultiple x y = x == y || mod y x /= 0 -- filter out multiples of x without filtering out x itself
         isMultipleOfX = map isMultiple $ take sqrtOfEstP odds  -- partially apply x values to sieve function
         isPrime x = all ($ x) isMultipleOfX -- test if x passes sieve

-- Calculates estimate of the nth prime. In practice, this is higher than the real number.
pOfN :: Floating a => a -> a
pOfN n = n * (log n + log (log n - 1))

main :: IO()
main = print $ last $ primes 10001