-- Priority Queue for O'Neill Sieve

module PQ (empty, deleteMin, insert, deleteMinAndInsert, findMin, minKey)
where

-- | I choose to use the priority queue functionality of
-- Data.Map, which is mostly sufficient for this
-- example. Note that deleteMinAndInsert is not supported by
-- Data.Map, so the "heap speedup" will not apply. More
-- damningly, note that there's a bunch of shenanigans to
-- get "MultiMap" functionality here. This probably slows
-- down this implementation substantially.

import qualified Data.Map as M

empty :: M.Map Integer [[Integer]]
empty = M.empty

deleteMin :: M.Map Integer [[Integer]] ->
             M.Map Integer [[Integer]]
deleteMin q =
  case M.deleteFindMin q of
    ((_, []), _) -> error "internal error: weird map entry"
    ((_, [_]), q') -> q'
    ((k, _ : xs), q') -> M.insert k xs q'

insert :: Integer -> [Integer] ->
          M.Map Integer [[Integer]] ->
          M.Map Integer [[Integer]]
insert k x q =
  case M.lookup k q of
    Nothing -> M.insert k [x] q
    Just xs -> M.insert k (x : xs) q

deleteMinAndInsert :: Integer -> [Integer] -> 
                      M.Map Integer [[Integer]] ->
                      M.Map Integer [[Integer]]
deleteMinAndInsert k v q =
  insert k v $ deleteMin q

findMin :: M.Map Integer [[Integer]] ->
           (Integer, [Integer])
findMin q =
  case M.findMin q of
    (_, []) -> error "internal error: empty map entry"
    (k, x : _) -> (k, x)

minKey :: M.Map Integer [[Integer]] -> Integer
minKey q = fst $ M.findMin q
