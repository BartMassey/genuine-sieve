-- Copyright © 2008 Richard Bird
-- Used without permission

-- "Genuine Sieve of Eratosthenes" in pure lists.
-- From the paper of the same title by Melissa E. O'Neill,
-- <http://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf>.

primes :: [Integer]
primes = 2:([3..] `minus` composites)
  where
    composites = union [multiples p | p <- primes]
  
multiples :: Integer -> [Integer]
multiples n = map (n*) [n..]
  
minus :: [Integer] -> [Integer] -> [Integer]
(x:xs) `minus` (y:ys) | x < y = x:(xs `minus` (y:ys))
                      | x == y = xs `minus` ys
                      | x > y = (x:xs) `minus` ys
_ `minus` _ = error "internal error: bad minus"
  
union :: [[Integer]] -> [Integer]
union = foldr merge []
  where
    merge (x:xs) ys = x:merge' xs ys
    merge _ _ = error "internal error: bad merge"
    merge' (x:xs) (y:ys) | x < y = x:merge' xs (y:ys)
                         | x == y = x:merge' xs ys
                         | x > y = y:merge' (x:xs) ys
    merge' _ _ = error "internal error: bad merge'"

main :: IO ()
main = do
  print $ length $ takeWhile (<2000000) $ primes
