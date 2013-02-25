-- Copyright © 2008 Richard Bird
-- Used without permission

-- "Genuine Sieve of Eratosthenes" in pure lists.
-- From the paper of the same title by Melissa E. O'Neill,
-- <http://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf>.

import Data.Word
import DefaultMain

primes :: [Word64]
primes = 2:([3..] `minus` composites)
  where
    composites = union [multiples p | p <- primes]
  
multiples :: Word64 -> [Word64]
multiples n = map (n*) [n..]
  
minus :: [Word64] -> [Word64] -> [Word64]
(x:xs) `minus` (y:ys) | x < y = x:(xs `minus` (y:ys))
                      | x == y = xs `minus` ys
                      | x > y = (x:xs) `minus` ys
_ `minus` _ = error "internal error: bad minus"
  
union :: [[Word64]] -> [Word64]
union = foldr merge []
  where
    merge (x:xs) ys = x:merge' xs ys
    merge _ _ = error "internal error: bad merge"
    merge' (x:xs) (y:ys) | x < y = x:merge' xs (y:ys)
                         | x == y = x:merge' xs ys
                         | x > y = y:merge' (x:xs) ys
    merge' _ _ = error "internal error: bad merge'"

main :: IO ()
main = defaultMain primes Nothing
