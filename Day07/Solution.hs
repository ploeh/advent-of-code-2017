module Day07 where

import Text.ParserCombinators.ReadP
import qualified Data.Set as Set
import Data.Tree
import Data.List

-- Parsing
-- Could probably have done the parsing without parser combinators, but I took
-- the opportunity to learn a bit about how these work

parseName :: ReadP String
parseName = munch (`elem` ['a'..'z'])

parseParent :: ReadP String
parseParent = parseName <* skipSpaces

parseWeight :: ReadP Int
parseWeight = read <$> between (char '(') (char ')') (many get) :: ReadP Int

parseChildren :: ReadP [String]
parseChildren = option [] $ string " -> " *> sepBy parseName (string ", ")

parseLine :: ReadS (String, Int, [String])
parseLine =
  readP_to_S
  $ pure (\p w cs -> (p, w, cs))
  <*> parseParent
  <*> parseWeight
  <*> (parseChildren <* eof)

-- Lines are made from (name, weight, childNames)
parseInput :: String -> [(String, Int, [String])]
parseInput =
  concatMap (fmap fst . filter ((== "") . snd)) . fmap parseLine . lines

-- Puzzle 1
solve1 :: (Foldable t, Ord a) => t (a, b, [a]) -> Set.Set a
solve1 inp = Set.difference (Set.fromList parents) (Set.fromList children)
  where
    (parents, children) =
      foldl (\(ps, css) (p, _, cs) -> (p : ps, cs ++ css)) ([], []) inp

-- Puzzle 2
buildForest :: Ord a => [(a, b, [a])] -> Forest (a, b)
buildForest inp = unfoldForest unf roots
  where
    rootNames = solve1 inp
    roots = findByNames rootNames
    findByNames names = filter (\(name, _, _) -> elem name names) inp
    unf (name, weight, childNames) = ((name, weight), findByNames childNames)

-- Enumerates unbalanced nodes, from most shallow to deep. At least given the
-- constraints of this puzzle, the last entry is the most specific unbalanced
-- node.
-- In other words, the output of this functions gives you the path to the
-- deepest unbalanced node.
findUnbalanced :: (Num b, Eq b) => Tree (a, b) -> [Tree (a, b)]
findUnbalanced (Node (name, weight) children) =
  let childWeights = fmap (sum . fmap snd) children
  in if length (nub childWeights) > 1
    then Node (name, weight) children : concatMap findUnbalanced children
    else []

suggestAdjustment :: (Ord b, Num b) => Tree (a, b) -> (a, Maybe b)
suggestAdjustment (Node (name, _) children) =
  let allChildWeights = fmap (sum . fmap snd) children
      nextChildWeights = fmap (\(Node (_, w) _) -> w) children
      mx = maximum allChildWeights
      mn = minimum allChildWeights
      diff = mx - mn
      tooHeavyNode =
        snd <$> find ((==) mx . fst) (zip allChildWeights nextChildWeights)
  in (name, fmap (subtract diff) tooHeavyNode)

solve2 :: (Ord a, Ord b, Num b) => [(a, b, [a])] -> Maybe b
solve2 =
  snd . last . fmap suggestAdjustment . concatMap findUnbalanced . buildForest

test :: IO Bool
test = do
  inp <- readFile "./example.txt"
  return $ and
    [
       Set.singleton "tknk" == solve1 ( parseInput inp),
       Just 60 == solve2 (parseInput inp)
    ]
