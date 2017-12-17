{-# LANGUAGE NumDecimals #-}
module Day17 where

import Data.List (foldl')

-- Puzzle 1

-- Perform one step of the algorithm. Instead of keeping track of the current
-- position, this version always returns a list that has the current position as
-- the head.
step :: Int -> [a] -> a -> [a]
step stepSize buffer value =
  let l = length buffer
      position = stepSize `mod` l
      (prefix, suffix) = splitAt (position + 1) buffer
  in value : suffix ++ prefix

run :: (Enum a, Num a) => Int -> a -> [a]
run stepSize steps = foldl' (step stepSize) [0] [1..steps]

solve1 :: (Enum a, Num a, Eq a) => Int -> a
solve1 stepSize = dropWhile (2017 /=) (run stepSize 2017) !! 1

-- Puzzle 2

-- Assume that 0 stays left-most. This optimized function only keeps track of
-- the current position, and which value is to the immediate right of 0. The
-- value to insert is always the same as the length (`l`) of the ring buffer.
step' :: Integral a => a -> (a, a) -> a -> (a, a)
step' stepSize (position, afterZero) l =
  let insertAfter = (position + stepSize) `mod` l
  in if insertAfter == 0
    then (insertAfter + 1, l)
    else (insertAfter + 1, afterZero)

-- Even with the optimization of step', this takes almost 3 minutes to run on
-- my old laptop. For the purposes of a one-shot problem like this one, however,
-- that's good enough for me.
solve2 :: Integral a => a -> a
solve2 stepSize = snd $ foldl' (step' stepSize) (0, 0) [1..50e6]

-- Tests
test :: Bool
test = and
  [
    [1 :: Int,0]                 == run 3 1,
    [2 :: Int,1,0]               == run 3 2,
    [3 :: Int,1,0,2]             == run 3 3,
    [4 :: Int,3,1,0,2]           == run 3 4,
    [5 :: Int,2,4,3,1,0]         == run 3 5,
    [6 :: Int,1,0,5,2,4,3]       == run 3 6,
    [7 :: Int,2,4,3,6,1,0,5]     == run 3 7,
    [8 :: Int,6,1,0,5,7,2,4,3]   == run 3 8,
    [9 :: Int,5,7,2,4,3,8,6,1,0] == run 3 9,
    ( 638 :: Int) == solve1   3,
    (1282 :: Int) == solve1 335, -- My puzzle input
    (1, 1) == step' 3 (0, 0) (1 :: Int),
    (1, 2) == step' 3 (1, 1) (2 :: Int),
    (2, 2) == step' 3 (1, 2) (3 :: Int),
    (2, 2) == step' 3 (2, 2) (4 :: Int),
    (1, 5) == step' 3 (2, 2) (5 :: Int),
    (5, 5) == step' 3 (1, 5) (6 :: Int),
    (2, 5) == step' 3 (5, 5) (7 :: Int),
    (6, 5) == step' 3 (2, 5) (8 :: Int),
    (1, 9) == step' 3 (6, 5) (9 :: Int)
  ]
