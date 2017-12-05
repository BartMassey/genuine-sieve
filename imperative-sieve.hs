{-# LANGUAGE FlexibleContexts #-}
-- Copyright © 2013 Bart Massey
-- [This work is licensed under the "MIT License"]
-- Please see the file COPYING in the source
-- distribution of this software for license terms.

-- Sieve of Eratosthenes

-- | This is a straightforward array-of-booleans Sieve
-- of Eratosthenes designed to be comparable with the
-- C version.

import Control.Monad.ST
import Data.Array.ST.Safe
import Data.Word
import DefaultMain

sieve :: Word64 -> ST s [Word64]
sieve n = do
  a <- newArray (3, n) False :: ST s (STUArray s Word64 Bool)
  ps <- sieveStep 3 a
  return (2 : ps)
  where
    sieveStep i a = do
      np <- nextPrime i
      case np of
        Nothing ->
          return []
        Just i' -> do
          markPrimes (i' + i') (i' * i')
          ps' <- sieveStep (i' + 2) a
          return (i' : ps')
      where
        nextPrime j 
          | j > n = 
            return Nothing
          | otherwise = do
              m <- readArray a j
              case m of
                True -> nextPrime (j + 2)
                False -> return (Just j)
        markPrimes k j
          | j > n =
            return ()
          | otherwise = do
              writeArray a j True
              markPrimes k (j + k)
          

primesLim :: Word64 -> [Word64]
primesLim n =
  runST $ sieve n

-- | Test the program's operation.
main :: IO ()
main = defaultMain Nothing (Just primesLim)
