module Day13 where

import Data.List (find)
import Data.Maybe (fromJust)
import Data.Bifunctor (bimap, first)

-- Parsing

-- Not particularly robust, but works with the input given
parseLine :: String -> (Int, Int)
parseLine = bimap read (read . tail) . break (== ':')

parseInput :: String -> [(Int, Int)]
parseInput = fmap parseLine . lines

-- Puzzle 1

-- Determines if a packet at picosecond `i` is caught, given a range. A security
-- scanner oscillates from 0 to (range - 1) and back, so every (range - 1) * 2
-- time, it returns to the start (index 0).
caught :: Integral a => a -> a -> Bool
caught i range =
  let l = (range - 1) * 2
  in 0 == i `mod` l

solve1 :: String -> Int
solve1 inp =
  let firewall = parseInput inp
      severity i = maybe 0 (* i) $ find (caught i) $ lookup i firewall
  in sum $ fmap (severity . fst) firewall

-- Puzzle 2

solve2 :: String -> Int
solve2 inp =
  let firewall = parseInput inp
      canPass delay =
        not $ or $ fmap (uncurry caught . first (+ delay)) firewall
  in fromJust $ find canPass [0..]

-- Tests
test :: Bool
test = and
  [
    24 == solve1 "0: 3\n1: 2\n4: 4\n6: 4\n",
    10 == solve2 "0: 3\n1: 2\n4: 4\n6: 4\n"
  ]
