module Day34 where

import Data.Maybe (mapMaybe)
import Data.Tuple (swap)
import Data.Ord (comparing)
import Data.List (delete, maximumBy)
import Data.Tree (Forest, Tree(..), unfoldForest)
import Control.Arrow ((&&&))
import GHC.Read (readPrec)
import Text.ParserCombinators.ReadPrec (lift)
import Text.ParserCombinators.ReadP (choice, char, munch)

-- Model

data Component a = Component
  { componentPorts :: (a, a)
  , componentIsInverted :: Bool }

leftPort :: Component a -> a
leftPort (Component (x, _) False) = x
leftPort (Component (_, x)  True) = x

rightPort :: Component a -> a
rightPort (Component (_, x) False) = x
rightPort (Component (x, _)  True) = x

instance Eq a => Eq (Component a) where
  Component x _ == Component y _ = x == y

instance Show a => Show (Component a) where
  show (Component (a, b) False) = show a ++  "/" ++ show b
  show (Component (a, b)  True) = show b ++ "\\" ++ show a

instance Read a => Read (Component a) where
  readPrec =
    lift $ choice
    [
      curry (flip Component False       ) <$> parseNumber <* char  '/' <*> parseNumber,
      curry (flip Component  True . swap) <$> parseNumber <* char '\\' <*> parseNumber
    ]
    where parseNumber = read <$> munch (`elem` ['0'..'9'])

invert :: Component a -> Component a
invert c = c { componentIsInverted = not $ componentIsInverted c }

tryFit :: Eq a => a -> Component a -> Maybe (Component a)
tryFit targetPort candidate = imp
  where
    invertedCandidate = invert candidate
    imp | targetPort == leftPort candidate = Just candidate
        | targetPort == leftPort invertedCandidate = Just invertedCandidate
        | otherwise = Nothing

componentStrength :: Num a => Component a -> a
componentStrength (Component (x, y) _) = x + y

bridgeStrength :: [Component Integer] -> Integer
bridgeStrength = sum . map componentStrength

-- Parsing

parseInput :: Read a => String -> [Component a]
parseInput = fmap read . lines

-- Puzzle 1

zeroes :: (Num a, Eq a) => [Component a] -> [Component a]
zeroes = mapMaybe $ tryFit 0

takeFromPool :: Eq a => [a] -> a -> (a, [a])
takeFromPool xs x = (x, delete x xs)

type ComponentAndPool a = (Component a, [Component a])

nextLayer :: Eq a => ComponentAndPool a -> (Component a, [ComponentAndPool a])
nextLayer (component, pool) =
  let fits = mapMaybe (tryFit $ rightPort component) pool
  in (component, takeFromPool pool <$> fits)

createForest :: (Num a, Eq a) => [Component a] -> Forest (Component a)
createForest components =
  unfoldForest nextLayer $ takeFromPool components <$> zeroes components

-- Adapted from https://stackoverflow.com/a/15344799/126014
listBranches :: Tree a -> [[a]]
listBranches (Node label []) = [[label]]
listBranches (Node label xs) = (label :) <$> concatMap listBranches xs

-- Takes about a minute to run...
solve1 :: String -> Integer
solve1 =
  maximum
  . fmap bridgeStrength
  . concatMap listBranches
  . createForest
  . parseInput

-- Puzzle 2

-- Takes about a minute to run...
solve2 :: String -> Integer
solve2 =
  bridgeStrength
  . maximumBy (comparing $ length &&& bridgeStrength)
  . concatMap listBranches
  . createForest
  . parseInput

-- Tests

example1 :: String
example1 = "0/2\n\
           \2/2\n\
           \2/3\n\
           \3/4\n\
           \3/5\n\
           \0/1\n\
           \10/1\n\
           \9/10"

test :: Bool
test = and
  [
    31 == solve1 example1,
    19 == solve2 example1
  ]
