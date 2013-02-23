-- Copyright © 2008 Melissa E. O'Neill
-- Used without permission

-- "Genuine Sieve of Eratosthenes" in pure lists.  From the
-- paper of the same title by Melissa E. O'Neill,
-- <http://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf>.
-- This is the version with evens struck but without the
-- full wheel, for comparison purposes. I choose to use the
-- priority queue functionality of Data.Map, which is
-- mostly sufficient for this example. Note that
-- deleteMinAndInsert is not supported by Data.Map, so
-- the "heap speedup" will not apply.

import qualified Data.Map as PQ

primes :: [Integer]
primes = 2 : sieve [3,5..]

deleteMinAndInsert :: Integer -> [Integer] -> 
                      PQ.Map Integer [Integer] ->
                      PQ.Map Integer [Integer]
deleteMinAndInsert k v q =
  PQ.insert k v $ PQ.deleteMin q

minKey :: PQ.Map Integer [Integer] -> Integer
minKey q = fst $ PQ.findMin q

sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (x0:xs0) = 
  x0 : sieve' xs0 (insertprime x0 xs0 PQ.empty)
  where
    insertprime p xs table = PQ.insert (p*p) (map (* p) xs) table
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
            (n, n':ns) = PQ.findMin table'

main :: IO ()
main = do
  print $ takeWhile (<100) primes
  print $ length $ takeWhile (<2000000) primes
