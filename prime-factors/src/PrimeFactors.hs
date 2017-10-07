module PrimeFactors (primeFactors) where
import Data.Numbers.Primes (primes, isPrime)
-- primes = seive [2..]
--   where seive (x:xs) = x:(seive [i | i <- xs, i `mod` x > 0])

primeFactors :: Integer -> [Integer]
primeFactors n = reverse $ primeFactors' [] n ps
  where ps = takeWhile (<=(fromIntegral $ truncate $ sqrt $ fromIntegral n)) primes

primeFactors' l 0 ps = l
primeFactors' l 1 ps = l
primeFactors' l n [] = n:l
primeFactors' l n (p:ps) =
  if n `mod` p == 0
  then primeFactors' (p:l) (n `div` p) (p:ps)
  else primeFactors' l n ps
