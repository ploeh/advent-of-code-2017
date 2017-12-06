module Day06 where

import Data.List (elemIndex, elemIndices)
import Data.Maybe (fromJust)
import qualified Data.Set as Set

-- Name stolen from Data.Sequence
adjust :: Foldable t => (a -> a) -> Int -> t a -> [a]
adjust f idx xs =
  let upd i x = if i == idx then f x else x
  in snd $ foldr (\x (i, acc) -> (i - 1, upd i x : acc)) (length xs - 1, []) xs

-- Name stolen from Data.Sequence
update :: Foldable t => a -> Int -> t a -> [a]
update x = adjust (const x)

normalizeIndex :: Foldable t => Int -> t a -> Int
normalizeIndex idx xs = idx `mod` length xs

-- Move the blocks and drip one of them into the next bank. This is the most
-- granular step; it's like sowing in Kalaha.
drip :: (Num a, Num b, Foldable t) => (Int, b, t a) -> (Int, b, [a])
drip (idx, blocks, xs) =
  let newIdx = normalizeIndex (idx + 1) xs
      xs' = adjust (+ 1) newIdx xs
  in (newIdx, blocks - 1, xs')

redist :: (Num a, Ord a) => [a] -> [a]
redist xs =
  let m = maximum xs
      idx = fromJust $ elemIndex m xs -- Ugly, but maximum is already unsafe...
      xs' = update 0 idx xs
      positiveBlocks (_, blocks, _) = 0 < blocks
      (_, _, banks) =
        head $ dropWhile positiveBlocks $ iterate drip (idx, m, xs')
  in banks

-- Totally stolen from https://stackoverflow.com/a/45880037/126014
hasDuplicates :: (Ord a) => [a] -> Bool
hasDuplicates list = length list /= length set
  where set = Set.fromList list

runUntilDuplicateFound :: (Num a, Ord a) => [[a]] -> [[a]]
runUntilDuplicateFound [] = []
runUntilDuplicateFound configs@(h : _) =
  let next = redist h
      configs' = next : configs
  in if hasDuplicates configs'
      then configs'
      else runUntilDuplicateFound configs'

 -- Subtract one because the first element in the list is the initial
 -- configration, and that doesn't count as a step
solve1 :: (Ord a, Num a) => [a] -> Int
solve1 banks = length (runUntilDuplicateFound [banks]) - 1

solve2 :: (Ord a, Num a) => [a] -> Int
solve2 banks = last $ elemIndices (head configs) configs
  where configs = runUntilDuplicateFound [banks]

test :: Bool
test = and
  [
    [2, 4, 1, 2] == redist ([0, 2, 7, 0]  :: [Int]),
    [3, 1, 2, 3] == redist ([2, 4, 1, 2]  :: [Int]),
    [0, 2, 3, 4] == redist ([3, 1, 2, 3]  :: [Int]),
    [1, 3, 4, 1] == redist ([0, 2, 3, 4]  :: [Int]),
    [2, 4, 1, 2] == redist ([1, 3, 4, 1]  :: [Int]),
    5 == solve1 ([0, 2, 7, 0] :: [Int]),
    4 == solve2 ([0, 2, 7, 0] :: [Int])
  ]
