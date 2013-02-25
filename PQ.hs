-- Copyright © 2013 Bart Massey
-- [This work is licensed under the "MIT License"]
-- Please see the file COPYING in the source
-- distribution of this software for license terms.

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
import Data.Word

empty :: M.Map Word64 [[Word64]]
empty = M.empty

deleteMin :: M.Map Word64 [[Word64]] ->
             M.Map Word64 [[Word64]]
deleteMin q =
  case M.deleteFindMin q of
    ((_, []), _) -> error "internal error: weird map entry"
    ((_, [_]), q') -> q'
    ((k, _ : xs), q') -> M.insert k xs q'

insert :: Word64 -> [Word64] ->
          M.Map Word64 [[Word64]] ->
          M.Map Word64 [[Word64]]
insert k x q =
  case M.lookup k q of
    Nothing -> M.insert k [x] q
    Just xs -> M.insert k (x : xs) q

deleteMinAndInsert :: Word64 -> [Word64] -> 
                      M.Map Word64 [[Word64]] ->
                      M.Map Word64 [[Word64]]
deleteMinAndInsert k v q =
  insert k v $ deleteMin q

findMin :: M.Map Word64 [[Word64]] ->
           (Word64, [Word64])
findMin q =
  case M.findMin q of
    (_, []) -> error "internal error: empty map entry"
    (k, x : _) -> (k, x)

minKey :: M.Map Word64 [[Word64]] -> Word64
minKey q = fst $ M.findMin q
