-- Copyright © 2013 Bart Massey
-- [This work is licensed under the "MIT License"]
-- Please see the file COPYING in the source
-- distribution of this software for license terms.

-- Priority Queue for O'Neill Sieve

module MPQ (empty, deleteMin, insert, deleteMinAndInsert, findMin, minKey)
where

-- | I choose to use the priority queue functionality of
-- Data.Map, which is mostly sufficient for this
-- example. Note that deleteMinAndInsert is not supported by
-- Data.Map, so the "heap speedup" will not apply.  Rather
-- than doing "multi-map" tricks, this version simply merges
-- lists that share the same key.

import qualified Data.Map as M
import Data.Word

-- | This merge keeps only one copy of
-- duplicate elements. It assumes that
-- neither list contains duplicates
-- to begin with.
merge :: Ord a => [a] -> [a] -> [a]
merge xs1 [] = xs1
merge [] xs2 = xs2
merge l1@(x1 : xs1) l2@(x2 : xs2) =
  case compare x1 x2 of
    LT -> x1 : merge xs1 l2
    EQ -> x1 : merge xs1 xs2
    GT -> x2 : merge l1 xs2

empty :: M.Map Word64 [Word64]
empty = M.empty

deleteMin :: M.Map Word64 [Word64] ->
             M.Map Word64 [Word64]
deleteMin q = M.deleteMin q

insert :: Word64 -> [Word64] ->
          M.Map Word64 [Word64] ->
          M.Map Word64 [Word64]
insert k xs q =
  case M.lookup k q of
    Nothing -> M.insert k xs q
    Just xs' -> M.insert k (merge xs xs') q

deleteMinAndInsert :: Word64 -> [Word64] -> 
                      M.Map Word64 [Word64] ->
                      M.Map Word64 [Word64]
deleteMinAndInsert k v q =
  insert k v $ deleteMin q

findMin :: M.Map Word64 [Word64] -> (Word64, [Word64])
findMin q = M.findMin q

minKey :: M.Map Word64 [Word64] -> Word64
minKey q = fst $ M.findMin q
