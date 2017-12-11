module Day11 where

import Data.Maybe (catMaybes)

-- Using isometric cube coordinates. See e.g.
-- http://www-cs-students.stanford.edu/~amitp/Articles/Hexagon2.html

data HexCoordinate a =
  HexCoordinate { hx :: a, hy :: a, hz :: a } deriving (Show, Eq)

hex :: Num a => a -> a -> HexCoordinate a
hex x y = HexCoordinate x y (0 - x - y)

stepDistance :: (Num a, Ord a) => HexCoordinate a -> HexCoordinate a -> a
stepDistance (HexCoordinate x1 y1 z1) (HexCoordinate x2 y2 z2) =
  maximum [abs (x1 - x2), abs (y1 - y2), abs (z1 - z2)]

origin :: Num a => HexCoordinate a
origin = hex 0 0

-- Puzzle 1

data HexDirection = N | NW | SW | S | SE | NE deriving (Show, Eq)

parseStep :: String -> Maybe HexDirection
parseStep  "n" = Just  N
parseStep "nw" = Just NW
parseStep "sw" = Just SW
parseStep  "s" = Just  S
parseStep "se" = Just SE
parseStep "ne" = Just NE
parseStep    _ = Nothing

-- Adopted from https://stackoverflow.com/a/4981265/126014
wordsBy :: (Char -> Bool) -> String -> [String]
wordsBy p s =
  case dropWhile p s of
    "" -> []
    s' -> w : wordsBy p s''
      where (w, s'') = break p s'

splitOn :: Char -> String -> [String]
splitOn delimiter = wordsBy (== delimiter)

-- Starting with `lines` in order to strip away trailing newline
parseInput :: String -> [HexDirection]
parseInput = catMaybes . fmap parseStep . concatMap (splitOn ',') . lines

step :: Num a => HexCoordinate a -> HexDirection -> HexCoordinate a
step (HexCoordinate x y _) direction =
  case direction of
    N  -> hex  x      (y + 1)
    NW -> hex (x - 1) (y + 1)
    SW -> hex (x - 1)  y
    S  -> hex  x      (y - 1)
    SE -> hex (x + 1) (y - 1)
    NE -> hex (x + 1)  y

solve1 :: (Num a, Ord a) => String -> a
solve1 = stepDistance origin . foldl step origin . parseInput

-- Puzzle 2

solve2 :: (Num a, Ord a) => String -> a
solve2 = maximum . fmap (stepDistance origin) . scanl step origin . parseInput

-- Tests

test :: Bool
test = and
  [
    (3 :: Int) == solve1 "ne,ne,ne",
    (0 :: Int) == solve1 "ne,ne,sw,sw",
    (2 :: Int) == solve1 "ne,ne,s,s",
    (3 :: Int) == solve1 "se,sw,se,sw,sw",
    (3 :: Int) == solve2 "ne,ne,ne",
    (2 :: Int) == solve2 "ne,ne,sw,sw",
    (2 :: Int) == solve2 "ne,ne,s,s",
    (3 :: Int) == solve2 "se,sw,se,sw,sw"
  ]
