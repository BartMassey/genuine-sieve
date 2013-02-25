-- Copyright © 2008 Melissa E. O'Neill
-- Used without permission

-- | "Genuine Sieve of Eratosthenes" in pure lists.  From the
-- paper of the same title by Melissa E. O'Neill,
-- <http://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf>.
-- This is the version with evens struck but without the
-- full wheel, for comparison purposes. The PQ is custom-
-- crafted: see the comments there for details.

import Data.Word
import DefaultMain
import PQ

primes :: [Word64]
primes = 2 : sieve [3,5..]

sieve :: [Word64] -> [Word64]
sieve [] = []
sieve (x0:xs0) = 
  x0 : sieve' xs0 (insertprime x0 xs0 empty)
  where
    insertprime p xs table = insert (p*p) (map (* p) xs) table
    sieve' [] _ = []
    sieve' (x:xs) table
      | nextComposite <= x = sieve' xs (adjust table)
      | otherwise = x : sieve' xs (insertprime x xs table)
      where
        nextComposite = minKey table
        adjust table'
          | n <= x = adjust (deleteMinAndInsert n' ns table')
          | otherwise = table'
          where
            (n, n':ns) = findMin table'

main :: IO ()
main = defaultMain primes Nothing
