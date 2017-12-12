module Day12 where

import Data.List (foldl', partition)
import qualified Data.Set as Set
import Text.ParserCombinators.ReadP

-- Parsing

parseNumber :: ReadP Int
parseNumber = read <$> munch (`elem` ['0'..'9'])

parseConnected :: ReadP [Int]
parseConnected = sepBy parseNumber (string ", ")

parseLine :: ReadS (Int, [Int])
parseLine =
  readP_to_S
  $ pure (\x xs -> (x, xs))
  <*> parseNumber
  <*  skipSpaces
  <*  string "<->"
  <*  skipSpaces
  <*> parseConnected
  <*  eof

parseInput :: String -> [(Int, [Int])]
parseInput =
  concatMap (fmap fst . filter ((== "") . snd)) . fmap parseLine . lines

-- Puzzle 1

-- Add new values if they're connected to the target set.
addNodes :: Ord a => Set.Set a -> (a, [a]) -> Set.Set a
addNodes s (n, ns) =
  if Set.member n s
    then Set.union s $ Set.fromList ns
    else s

-- Do one pass over the entire input, adding values that are connected with
-- elements already in the set.
passOnce :: (Ord a, Foldable t) => Set.Set a -> t (a, [a]) -> Set.Set a
passOnce = foldl addNodes

-- Do an arbitrary number of passes over the input until the output is stable
-- (no longer changes). This may not be a generally scalable implementation, but
-- seems to be faster than the alternative described below for the particular
-- input I got.
passUntilStable :: (Foldable t, Ord a) => Set.Set a -> t (a, [a]) -> Set.Set a
passUntilStable s inp =
  let newSet = passOnce s inp
  in if Set.size newSet == Set.size s
    then s
    else passUntilStable newSet inp

-- The first puzzle can also be solved using the `connect` function I used for
-- puzzle 2, but it seems like this n-pass solution that I arrived at first is
-- faster, at least for my input, so I'm keeping it.
-- For the record, however, here's the alternative implementation:
-- solve1 inp =
--   let groups = foldl' connect [] $ parseInput inp
--       group0 = find (Set.member 0) groups
--   in sum $ fmap length group0
solve1 :: String -> Int
solve1 = length . passUntilStable (Set.singleton 0) . parseInput

-- Puzzle 2

-- Given a list of sets of connected values, as well as a new connection,
-- attempt to find sets that connect via the new connection. Connect all those
-- sets that are now connected, and return the new list of sets.
connect :: Ord a => [Set.Set a] -> (a, [a]) -> [Set.Set a]
connect sets (x, xs) =
  let s = Set.fromList (x : xs)
      (disconnected, connected) = partition (Set.null . Set.intersection s) sets
  in Set.unions (s : connected) : disconnected

solve2 :: String -> Int
solve2 = length . foldl' connect [] . parseInput

-- Tests

test :: IO Bool
test = do
  inp <- readFile "./example.txt"
  return $ and
    [
      6 == solve1 inp,
      2 == solve2 inp
    ]
