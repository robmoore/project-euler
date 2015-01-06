-- A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
--
-- a^2 + b^2 = c^2
--
-- For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.
--
-- There exists exactly one Pythagorean triplet for which a + b + c = 1000.
-- Find the product abc.

-- From http://mathforum.org/dr.math/faq/faq.pythag.triples.html:
-- a = (r^2 - s^2)d,
-- b = 2rsd,
-- c = (r^2 + s^2)d,
-- r > s > 0 and d > 0 are whole numbers,
-- r - s is odd, and
-- the greatest common divisor of r and s is 1.

module Prob9 where

findTriple :: [Integer]
findTriple = head [ [a, b, c] | r <- [1..l], s <- [1..l], d <- [1..l],
                        s < r, odd (r - s), gcd r s == 1,
                        let a = calcA r s d, let b = calcB r s d, let c = calcC r s d,
                        a + b + c == n]
    where n = 1000
          l = floor $ sqrt $ fromIntegral n
          calcA r s d = (r ^ 2 - s ^ 2) * d
          calcB r s d = 2 * r * s * d
          calcC r s d = (r ^ 2 + s ^ 2) * d

main :: IO ()
main = print $ product findTriple
