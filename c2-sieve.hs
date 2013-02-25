-- Copyright © 2013 Contributors to the c2 Wiki <http://c2.com/wiki>
-- Used without permission.

-- This is an implementation of "The Genuine Sieve of Eratosthenes"
-- from the c2 wiki. It is essentially Bird's algorithm, with some
-- serious optimizations.

union :: [Integer] -> [Integer] -> [Integer]
union xs@(x:t) ys@(y:u) = 
  case (compare x y) of
    LT -> x : union t ys
    EQ -> x : union t u
    GT -> y : union xs u
union xs ys = xs ++ ys

minus :: [Integer] -> [Integer] -> [Integer]
minus xs@(x:t) ys@(y:u) = 
  case (compare x y) of
    LT -> x : minus t ys
    EQ -> minus t u
    GT -> minus xs u
minus xs _ = xs

{-# OPTIONS_GHC -O2 -fno-cse #-}
primes :: [Integer]
primes =
  2 : ([3,5..] `minus` unionAll [[p*p, p*p+2*p..] | p <- primes'])
  where
    primes' =
      3 : ([5,7..] `minus` unionAll [[p*p, p*p+2*p..] | p <- primes'])
    unionAll ((x:xs):t) =
      x : union xs (unionAll (pairs t))
    unionAll _ = 
      error "internal error: unionAll ran off end"
    pairs ((x:xs):ys:t) = 
      (x : union xs ys) : pairs t
    pairs _ = 
      error "internal error: pairs ran off end"

main :: IO ()
main = do
  print $ length $ takeWhile (<2000000) $ primes
