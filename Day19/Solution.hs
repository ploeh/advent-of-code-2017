module Day19 where

import Data.Char (isLetter)
import Data.List (find, unfoldr, dropWhileEnd)
import Prelude hiding (Left, Right)

-- The solution presented here is slow. Both solutions take about a dozen
-- seconds to run. I can't say that I'm surprised by this, as it models the
-- entire grid of characters as an association list implemented by the standard
-- Haskell linked list. This means that every time the functions need to find a
-- particular coordinate, it'll need to scan most of the grid.
-- I strongly suspect that it'd be possible to optimise this solution by using
-- a faster data structure, like a map. This should, I hypothesise, be possible
-- without changing much of the underlying approach.
-- Given, however, that this solves both my puzzles in less than a minute, I
-- don't feel like tuning this.

-- Parsing

index2d :: [[t]] -> [((Integer, Integer), t)]
index2d =
  concatMap (\(j, l) -> fmap (\(i, k) -> ((i, j), k)) l)
  . zip [0..]
  . fmap (zip [0..])

parseInput :: String -> [((Integer, Integer), Char)]
parseInput = index2d . lines

-- Puzzle 1

line :: Eq y => y -> [((x, y), a)] -> [((x, y), a)]
line y = filter ((== y) . snd . fst)

column :: Eq x => x -> [((x, y), a)] -> [((x, y), a)]
column x = filter ((== x) . fst . fst)

findEntry :: (Num y, Eq y) => [((x, y), Char)] -> Maybe ((x, y), Char)
findEntry = find ((== '|') . snd) . line 0

data Direction = Up | Down | Left | Right deriving (Show, Eq)

-- Adapted from https://stackoverflow.com/a/22472610/126014
takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ [] = []
takeUntil p (x:xs) = x : if p x then takeUntil p xs else []

takeUntilNextTurn :: [(a, Char)] -> [(a, Char)]
takeUntilNextTurn = takeUntil ((/= '+') . snd)

follow :: (Ord x, Ord y) => (x, y) -> Direction -> [((x, y), Char)] -> [((x, y), Char)]
follow (x, y)    Up = takeUntilNextTurn . dropWhile ((> y) . snd . fst) . reverse . column x
follow (x, y)  Down = takeUntilNextTurn . dropWhile ((< y) . snd . fst)           . column x
follow (x, y)  Left = takeUntilNextTurn . dropWhile ((> x) . fst . fst) . reverse .   line y
follow (x, y) Right = takeUntilNextTurn . dropWhile ((< x) . fst . fst)           .   line y

directionsFrom :: (Num x, Num y) => (x, y) -> [((x, y), Direction)]
directionsFrom (x, y) =
  [
                         ((x, y - 1), Up),
    ((x - 1, y), Left)                   , ((x + 1, y), Right),
                       ((x, y + 1), Down)
  ]

findNextDirection :: (Num x, Num y, Eq x, Eq y) =>
                     (x, y) -> (x, y) -> [((x, y), Char)]
                     -> Maybe ((x, y), Direction)
findNextDirection penultimatePoint ultimatePoint grid = do
  let directions = directionsFrom ultimatePoint
  let candidates =
        filter (\(coord, c) -> c /= ' ' && coord `elem` fmap fst directions) grid
  (checkedPenultimatePoint, _) <- find ((==) penultimatePoint . fst) candidates
  (next, _) <- find ((/=) checkedPenultimatePoint . fst) candidates
  forwardDirection <- lookup next directions
  return (next, forwardDirection)

lastTwo :: [a] -> Maybe (a, a)
lastTwo xs =
  case reverse xs of
    []                       -> Nothing
    [_]                      -> Nothing
    (ultimate:penultimate:_) -> Just (penultimate, ultimate)

turnAndFollow :: (Ord x, Ord y, Num x, Num y) =>
                 [((x, y), b)] -> [((x, y), Char)] -> Maybe [((x, y), Char)]
turnAndFollow previousLeg grid = do
  van <- lastTwo previousLeg
  (next, direction) <- findNextDirection (fst $ fst van) (fst $ snd van) grid
  return $ follow next direction grid

run :: (Num x, Num y, Ord x, Ord y) =>
       [((x, y), Char)] -> Maybe [[((x, y), Char)]]
run grid = do
  (entryPoint, _) <- findEntry grid
  let firstLeg = follow entryPoint Down grid
  let step leg =
        if '+' == snd (last leg)
          then do
            newLeg <- turnAndFollow leg grid
            return (newLeg, newLeg)
          else Nothing
  return $ firstLeg : unfoldr step firstLeg

solve1 :: String -> String
solve1 = filter isLetter . fmap snd . concat . concat . run . parseInput

-- Puzzle 2

solve2 :: String -> Int
solve2 =
  length . dropWhileEnd (not . isLetter) . fmap snd . concat . concat . run . parseInput

-- Tests

example :: String
example = "     |          \n\
           \     |  +--+    \n\
           \     A  |  C    \n\
           \ F---|----E|--+ \n\
           \     |  |  |  D \n\
           \     +B-+  +--+ \n\
           \                \n\
           \"

test :: Bool
test = and
  [
    "ABCDEF" == solve1 example,
    38 == solve2 example
  ]
