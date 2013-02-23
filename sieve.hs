-- "Genuine Sieve of Eratosthenes"

-- | This is an implementation of what I believe to be "The
-- Genuine Sieve of Eratosthenes". The idea is taken from
-- the paper of this title by Melissa E. O'Neill,
-- J. Functional Programming 19:1, January 2009 
-- <http://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf>.
-- However, the implementation is my own, from scratch.
-- 
-- The basic idea of this implementation is to represent the
-- various sieves as infinite lists, and the whole sieve as
-- a 'Set' of these lists. Since 'Data.Set' provides
-- reasonably efficient routines for treating a 'Set' as a
-- min-heap, and since the default 'Ord' instance on lists
-- works just the way we want, we just walk up the sieve
-- dropping no-longer-useful values off the front of the
-- lists.
-- 
-- Comparison of the performance of this code with
-- implementations from the paper shows it to be slower, but
-- not dramatically so.

import Data.Set

-- | "Advance" all the lists in the set by dropping
-- any initial prefix less than or equal to 'n' from each.
advance :: Integer -> Set [Integer] -> Set [Integer]
advance n fs =
  case deleteFindMin fs of
    (m : ms, fs') 
      | m <= n -> advance n (ms `insert` fs')
      | m > n -> fs
    _ -> error "internal error: bad advance"

-- | Actual Sieve. Takes a starting point and an
-- an initial filter list, which may not be empty.
soe :: Integer -> Set [Integer] -> [Integer]
soe n filters =
  case findMin filters of
    (m : _) 
      | m < n -> error "internal error: unadvanced filter"
      | m == n -> soe (n + 2) (advance n filters)
    _ -> 
      n : soe (n + 2) ([n * n, (n + 2) * n ..] `insert` filters)
  
-- | Limit Sieve. Takes advantage of the known upper
-- bound to cut runtime by approximately half.
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
      then [n * n, (n + 2) * n ..] `insert` filters
      else filters
  
-- | Infinite list of primes.
primes :: [Integer]
primes = 2 : 3 : soe 5 (singleton [9, 15 ..])

-- | List of primes less than or equal to 'n'.
primesLim :: Integer -> [Integer]
primesLim n = 2 : 3 : soeLim 5 n (singleton [9, 15 ..])

-- | Test the program's operation.
main :: IO ()
main =
  -- print $ length $ takeWhile (<2000000) $ primes
  print $ length $ primesLim 1999999
