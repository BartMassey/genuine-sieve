-- Copyright © 2013 Bart Massey
-- [This work is licensed under the "MIT License"]
-- Please see the file COPYING in the source
-- distribution of this software for license terms.

-- | Fast primality testing for "medium-sized" primes.
-- Intended for proving that 8675309 is prime.

import Data.Set hiding (foldr, map, foldl')
import Data.List (mapAccumL)
import Data.Word
import System.Environment
import System.Exit

soe :: Integral a => Maybe a -> [a] -> [a]
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

primes0 :: [Int]
primes0 = 2 : soe Nothing [3, 5 ..]

wheel :: Int -> ([Word64], [Word64])
wheel np =
  let wps = take np primes0 in
  let wp1 = primes0 !! np in
  let wheelRange = [wp1 + 1 .. product wps + wp1 + 1] in
  let ws0 = [ cp | cp <- wheelRange, all (nondiv cp) wps ] in
  let ws = snd $ mapAccumL (\a x -> (x, x - a)) wp1 ws0 in
  (map fromIntegral wps, spin (cycle ws) (fromIntegral wp1))
  where
    nondiv cp wp =
      cp `mod` wp /= 0
    -- From O'Neill
    spin (x : xs) n = 
      n : spin xs (n + fromIntegral x)
    spin _ _ = error "internal error: spin stopped"

primes :: Int -> [Word64]
primes n =
  let (wps, ws) = wheel n in
  wps ++ soe Nothing ws

main :: IO ()
main = do
  [cps] <- getArgs
  let cp = read cps
  let ps = takeWhile (\p -> p * p <= cp) $ primes 6
  if any (\p -> cp `mod` p == 0) ps
    then do
      putStrLn $ cps ++ " is composite"
      exitFailure
    else do
      putStrLn $ cps ++ " is prime"
      exitSuccess
