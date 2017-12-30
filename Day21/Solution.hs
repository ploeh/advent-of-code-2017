module Day21 where

import Data.List (unfoldr, dropWhileEnd, intercalate, transpose, nub, unzip4)
import Data.Maybe (catMaybes)
import Data.Bifunctor (bimap)
import Data.Semigroup (Endo(..), (<>))
import qualified Data.Map.Strict as Map

-- Parsing

trim :: String -> String
trim = dropWhile (== ' ') . dropWhileEnd (== ' ')

removeArrow :: String -> String
removeArrow = dropWhile (== '>') . dropWhile (== '=')

parseLine :: String -> (String, String)
parseLine = bimap trim (trim . removeArrow) . break (== '=')

parseInput :: String -> [(String, String)]
parseInput = fmap parseLine . lines

-- Puzzle 1

-- Adopted from https://stackoverflow.com/a/4981265/126014
wordsBy :: (Char -> Bool) -> String -> [String]
wordsBy p s =
  case dropWhile p s of
    "" -> []
    s' -> w : wordsBy p s''
      where (w, s'') = break p s'

splitOn :: Char -> String -> [String]
splitOn delimiter = wordsBy (== delimiter)

-- Mostly used for visualizing patterns. Use with putStrLn to actually render
-- the line breaks.
visualize :: String -> String
visualize = unlines . splitOn '/'

flipHorizontal :: String -> String
flipHorizontal = intercalate "/" . reverse . splitOn '/'

flipVertical :: String -> String
flipVertical = intercalate "/" . fmap reverse . splitOn '/'

rotateClockwise :: String -> String
rotateClockwise = intercalate "/" . fmap reverse . transpose . splitOn '/'

rotateCounterclockwise :: String -> String
rotateCounterclockwise = intercalate "/" . reverse . transpose . splitOn '/'

transformations :: String -> [String]
transformations s = nub $ ts <*> [s]
  where
    doOrDoNot f = [Endo f, mempty]
    ts =
      fmap appEndo
      $   (\x y z æ -> x <> y <> z <> æ)
      <$> doOrDoNot flipHorizontal
      <*> doOrDoNot flipVertical
      <*> doOrDoNot rotateClockwise
      <*> doOrDoNot rotateCounterclockwise

expandRules :: [(String, t)] -> [(String, t)]
expandRules rules = [(t, outp) | (inp, outp) <- rules, t <- transformations inp]

mapOfRules :: [(String, a)] -> Map.Map String a
mapOfRules = Map.fromList . expandRules

-- Stolen from https://stackoverflow.com/a/12882583/126014
chunksOf :: Int -> [a] -> [[a]]
chunksOf n =
  Prelude.takeWhile (not . null) . Data.List.unfoldr (Just . Prelude.splitAt n)

toTuples :: [t] -> [(t, t)]
toTuples (x:y:t) = (x,y) : toTuples t
toTuples      _  = []

tuplesToSquare :: ((Char, Char), (Char, Char)) -> String
tuplesToSquare ((p00, p01), (p10, p11)) = [p00, p10, '/', p01, p11]

split2x2 :: String -> [String]
split2x2 =
  fmap tuplesToSquare
  . toTuples
  . concatMap (uncurry zip)
  . toTuples
  . splitOn '/'

toTriples :: [t] -> [(t, t, t)]
toTriples (x:y:z:t) = (x,y,z) : toTriples t
toTriples        _  = []

triplesToSquare :: ((Char, Char, Char), (Char, Char, Char), (Char, Char, Char))
                   -> String
triplesToSquare ((p00, p01, p02), (p10, p11, p12), (p20, p21, p22)) =
  [p00, p10, p20, '/', p01, p11, p21, '/', p02, p12, p22]

split3x3 :: String -> [String]
split3x3 =
  fmap triplesToSquare
  . toTriples
  . concatMap (\(x, y, z) -> zip3 x y z)
  . toTriples
  . splitOn '/'

join3x3 :: ([String], [String], [String]) -> [String]
join3x3 (xs, ys, zs) = xs ++ ["/"] ++ ys ++ ["/"] ++ zs

toQuadruples :: [t] -> [(t, t, t, t)]
toQuadruples (x:y:z:æ:t) = (x,y,z,æ) : toQuadruples t
toQuadruples          _  = []

join4x4 :: ([String], [String], [String], [String]) -> [String]
join4x4 (xs, ys, zs, æs) = xs ++ ["/"] ++ ys ++ ["/"] ++ zs ++ ["/"] ++ æs

enhanceSquares :: Ord k => Map.Map k a -> (t -> [k]) -> t -> [a]
enhanceSquares rules split = catMaybes . fmap (`Map.lookup` rules) . split

joinSquares :: Foldable t =>
               ([String] -> [a]) -> ([a] -> b) -> (b -> t String) -> [String]
               -> String
joinSquares toTpls unzp append squares =
  let size = floor $ sqrt $ fromIntegral $ length squares
      rows = chunksOf size squares
  in intercalate "/"
     $ fmap (concat . append . unzp . concatMap (toTpls . splitOn '/')) rows

enhance :: Map.Map String String -> String -> String
enhance rules grid | even $ length $ filter (/= '/') grid =
  joinSquares    toTriples unzip3 join3x3 $ enhanceSquares rules split2x2 grid
enhance rules grid =
  joinSquares toQuadruples unzip4 join4x4 $ enhanceSquares rules split3x3 grid

initialPattern :: String
initialPattern = ".#./\
                 \..#/\
                 \###"

pixelsOnAfter :: Int -> Map.Map String String -> Int
pixelsOnAfter iterations rules =
  length
  $ filter (== '#') $ iterate (enhance rules) initialPattern !! iterations

solve1 :: String -> Int
solve1 = pixelsOnAfter 5 . mapOfRules . parseInput

-- Puzzle 2

solve2 :: String -> Int
solve2 = pixelsOnAfter 18 . mapOfRules . parseInput

-- Tests

example1 :: String
example1 = "../.# => ##./#../...\n\
           \.#./..#/### => #..#/..../..../#..#"

test :: Bool
test = and
  [
    12 == pixelsOnAfter 2 (mapOfRules $ parseInput example1)
  ]
