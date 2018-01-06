{-# LANGUAGE NumDecimals #-}
module Day16 where

import Data.Array (Array, Ix, listArray, ixmap, bounds, (!), (//), elems)
import Data.Maybe (fromJust)
import Data.List (elemIndex, foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Text.ParserCombinators.ReadP

data Move a =
  Spin Int | Exchange Int Int | Partner a a deriving (Show, Eq)

-- Parsing

parseNumber :: ReadP Int
parseNumber = read <$> munch (`elem` ['0'..'9'])

parseSpin :: ReadP (Move a)
parseSpin = fmap Spin $ char 's' *> parseNumber

parseExchange :: ReadP (Move a)
parseExchange =
  pure Exchange <*> (char 'x' *> parseNumber) <*> (char '/' *> parseNumber)

parsePartner :: ReadP (Move Char)
parsePartner = pure Partner <*> (char 'p' *> get) <*> (char '/' *> get)

parseMove :: ReadP (Move Char)
parseMove = choice [parseSpin, parseExchange, parsePartner]

-- Starting with `lines` in order to strip away trailing newline
parseInput :: String -> [Move Char]
parseInput =
  concatMap (concatMap fst . filter ((== "") . snd)) . fmap parseLine . lines
  where
    parseMoves = sepBy parseMove (char ',') <* eof
    parseLine = readP_to_S parseMoves

-- Puzzle 1

createArray :: [e] -> Array Int e
createArray xs = listArray (0, length xs - 1) xs

spin :: Int -> Array Int a -> Array Int a
spin l arr =
  let al = length arr
      normalizeIndex i = i `mod` al
  in ixmap (bounds arr) (normalizeIndex . (+ (al - l))) arr

exchange :: Ix i => (i, i) -> Array i a -> Array i a
exchange (i, j) arr =
  let x = arr ! i
      y = arr ! j
  in arr // [(i, y), (j, x)]

partner :: Eq a => (a, a) -> Array Int a -> Array Int a
partner (x, y) arr =
  let values = elems arr
      i = fromJust $ elemIndex x values
      j = fromJust $ elemIndex y values
  in exchange (i, j) arr

evalMove :: Eq a => Move a -> Array Int a -> Array Int a
evalMove (Spin l)       = spin l
evalMove (Exchange i j) = exchange (i, j)
evalMove (Partner x y)  = partner (x, y)

evalMoves :: (Eq a, Foldable t) => t (Move a) -> Array Int a -> Array Int a
evalMoves moves arr = foldl' (flip evalMove) arr moves

solve1 :: String -> String
solve1 inp = elems $ evalMoves (parseInput inp) $ createArray ['a'..'p']

-- Puzzle 2

memoizedDance :: (Foldable t, Ord a) =>
                 t (Move a) ->
                 (Array Int a, Map (Array Int a) (Array Int a)) ->
                 (Array Int a, Map (Array Int a) (Array Int a))
memoizedDance moves (arr, m) =
  case Map.lookup arr m of
    Just res -> (res, m)
    Nothing ->
      let res = evalMoves moves arr
          m' = Map.insert arr res m
      in (res, m')

repeatDance :: (Ord a, Foldable t, Eq n, Num n) =>
               t (Move a) -> Array Int a -> n -> [a]
repeatDance moves arr iterations = elems $ fst $ imp (arr, Map.empty) 0
  where imp res counter | counter == iterations = res
        imp res counter =
          let res' = memoizedDance moves res
          in res' `seq` imp res' $ counter + 1

-- Takes 2-3 hours to run
solve2 :: String -> String
solve2 inp =
  let moves = parseInput inp
      arr = createArray ['a'..'p']
  in repeatDance moves arr (1e9 :: Integer)
