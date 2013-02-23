-- Copyright © 2013 Bart Massey
-- "Genuine Sieve of Eratosthenes"

import Data.Set

soe :: Integer -> Set [Integer] -> [Integer]
soe n filters =
  case findMin filters of
    (m : _) 
      | m < n -> error "internal error: unadvanced filter"
      | m == n -> soe (n + 2) (advance filters)
    _ -> 
      n : soe (n + 2) ([3 * n, 5 * n ..] `insert` filters)
  where
    advance fs =
      case deleteFindMin fs of
        (m : ms, fs') 
          | m <= n -> advance (ms `insert` fs')
          | m > n -> fs
        _ -> error "internal error: bad advance"
  
primes :: [Integer]
primes = 2 : 3 : soe 5 (singleton [9, 15 ..])

main :: IO ()
main = print $ last $ takeWhile (< 2000000) primes
