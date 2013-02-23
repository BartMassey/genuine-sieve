-- Copyright © 2013 Bart Massey
-- "Genuine Sieve of Eratosthenes"

import Data.Set

advance :: Integer -> Set [Integer] -> Set [Integer]
advance n fs =
  case deleteFindMin fs of
    (m : ms, fs') 
      | m <= n -> advance n (ms `insert` fs')
      | m > n -> fs
    _ -> error "internal error: bad advance"

soe :: Integer -> Set [Integer] -> [Integer]
soe n filters =
  case findMin filters of
    (m : _) 
      | m < n -> error "internal error: unadvanced filter"
      | m == n -> soe (n + 2) (advance n filters)
    _ -> 
      n : soe (n + 2) ([3 * n, 5 * n ..] `insert` filters)
  
soeLim :: Integer -> Integer -> Set [Integer] -> [Integer]
soeLim n lim _ | n > lim =
  []
soeLim n lim filters =
  case findMin filters of
    (m : _) 
      | m < n -> error "internal error: unadvanced filter"
      | m == n -> soeLim (n + 2) lim (advance n filters)
    _ -> 
      n : soeLim (n + 2) lim filters'
  where
    filters' =
      if n * n <= lim
      then [3 * n, 5 * n ..] `insert` filters
      else filters
  
primes :: [Integer]
primes = 2 : 3 : soe 5 (singleton [9, 15 ..])

primesLim :: Integer -> [Integer]
primesLim n = 2 : 3 : soeLim 5 n (singleton [9, 15 ..])

main :: IO ()
main = do
  print $ length $ takeWhile (<2000000) $ primes
  print $ length $ primesLim 1999999
