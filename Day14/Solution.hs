module Day14 where

import Data.Bifunctor (first)
import Data.List (partition, foldl')
import Numeric (readHex)
import Text.Printf (printf)
import KnotHash (calculateKnotHash)
import qualified Data.Set as Set

-- Puzzle 1

two :: Integer
two = 2

-- Grid size, 0-based
gridSize :: Integer
gridSize = 127

hexToBinary :: String -> String
hexToBinary =
  let collapseParsed = concatMap fst . filter ((== "") . snd)
      toBinary :: Integer -> String
      toBinary = printf "%0128b"
  in collapseParsed . fmap (first toBinary) . readHex

grid :: String -> [String]
grid inp =
  let hashInput i = inp ++ "-" ++ show i
  in fmap (hexToBinary . calculateKnotHash . hashInput) [0..gridSize]

solve1 :: String -> Int
solve1 = length . filter (== '1') . concat . grid

-- Puzzle 2

index2d :: (Enum a, Enum b, Num a, Num b) => [[t]] -> [((a, b), t)]
index2d =
  concatMap (\(i, l) -> fmap (\(j, k) -> ((i, j), k)) l)
  . zip [0..]
  . fmap (zip [0..])

adjacentNeighbors :: (Num a, Num b) => (a, b) -> [(a, b)]
adjacentNeighbors (x, y) =
  [
                (x, y + 1),
    (x - 1, y), (x, y)    , (x + 1, y),
                (x, y - 1)
  ]

-- Given a list of sets of adjacent squares, as well as a new square, attempt to
-- find sets that are adjacent. Connect all those sets that are now connected,
-- and return the new list of sets.
connect :: (Num a, Num b, Ord a, Ord b) =>
           Set.Set (a, b) -> [Set.Set (a, b)] -> (a, b) -> [Set.Set (a, b)]
connect g sets square =
  let s = Set.intersection g $ Set.fromList $ adjacentNeighbors square
      (disconnected, connected) = partition (Set.null . Set.intersection s) sets
  in Set.unions (s : connected) : disconnected

solve2 :: String -> Int
solve2 inp =
  let g = Set.fromAscList
          $ fmap fst
          $ filter ((== '1') . snd)
          $ index2d
          $ grid inp
      regions = foldl' (connect g) [] g :: [Set.Set (Integer, Integer)]
  in length regions
