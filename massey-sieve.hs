-- Copyright © 2013 Bart Massey
-- [This work is licensed under the "MIT License"]
-- Please see the file COPYING in the source
-- distribution of this software for license terms.

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
-- implementations from the paper shows it to be very
-- comparable. The c2 implementation, on the other hand...

import Data.Set
import Data.Word
import DefaultMain

-- | Sieve. Takes a list of candidate primes (maybe produced
-- by a wheel). If a limit is supplied, this acts as a limit
-- sieve, taking advantage of the known upper bound to cut
-- runtime by approximately half.
soe :: Maybe Word64 -> [Word64] -> [Word64]
soe _ [] = []
soe limit (n : ns) =
  n : soe' ns (singleton (makeStrikes n ns))
  where
    makeStrikes x xs =
      Prelude.map (x *) (x : xs)
    soe' [] _ = []
    soe' xs@(x' : xs') strikes =
      case Data.Set.null strikes of
        True -> xs
        False ->
          case deleteFindMin strikes of
            ([], strikes') ->
              soe' xs strikes'
            (c : cs, strikes') ->
              case c `compare` x' of
                LT -> soe' xs (insert cs strikes')
                EQ -> soe' xs' (insert cs strikes')
                GT -> x' : soe' xs' limitStrikes
              where
                limitStrikes =
                  case limit of
                    Just l | x' * x' > l -> strikes
                    _ -> insert (makeStrikes x' xs') strikes
  
-- | Infinite list of primes.
primes :: [Word64]
primes = 2 : soe Nothing [3, 5 ..]

-- | List of primes less than or equal to 'n'.
primesLim :: Word64 -> [Word64]
primesLim n = 2 : soe (Just n) [3, 5 .. n]

-- | Test the program's operation.
main :: IO ()
main = defaultMain (Just primes) (Just primesLim)
