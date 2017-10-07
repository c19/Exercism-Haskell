import PrimeFactors (primeFactors, primes)

main :: IO ()
main = print $ takeWhile (<=901255) primes
