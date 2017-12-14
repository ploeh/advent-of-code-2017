module KnotHash (calculateKnotHash) where

-- Copied from Day 10, but tuned to run orders of magnitude faster

import Data.Tuple (swap)
import Data.Bits (Bits, xor)
import Data.Bifunctor (first)
import Data.List (unfoldr, foldl')
import Text.Printf (printf)

-- Focused cycle
-- Favoured the term 'cycle' over 'ring', in order to avoid confusion with
-- algebraic structures like rings, semirings, and so on.

data FocusedCycle a =
  FocusedCycle { fcPosition :: Int, fcData :: [a] } deriving (Show, Eq)

normalizeIndex :: [a] -> Int -> Int
normalizeIndex l idx = idx `mod` length l

moveFocus :: Int -> FocusedCycle a -> FocusedCycle a
moveFocus n fc@(FocusedCycle p d) = fc { fcPosition = normalizeIndex d $ p + n }

enumerateFromPosition :: Int -> [a] -> [a]
enumerateFromPosition i = uncurry (++) . swap . splitAt i

step :: (Int, FocusedCycle a) -> Int -> (Int, FocusedCycle a)
step (skipSize, fc@(FocusedCycle p d)) l =
  let (twisted, rest) = first reverse $ splitAt l $ enumerateFromPosition p d
      newData = enumerateFromPosition (length d - p) $ twisted ++ rest
      -- Calculation is done. Move the focus.
      fc' = moveFocus (skipSize + l) fc
  in (skipSize + 1, fc' { fcData = newData })

runRound :: Foldable t => (Int, FocusedCycle a) -> t Int -> (Int, FocusedCycle a)
runRound = foldl' step

initialConfig :: (Int, FocusedCycle Int)
initialConfig = (0, FocusedCycle 0 [0..255])

lengthsFromAscii :: String -> [Int]
lengthsFromAscii = fmap fromEnum

standardSuffix :: [Int]
standardSuffix = [17, 31, 73, 47, 23]

calculateSparseHash :: Foldable t => t Int -> (Int, FocusedCycle Int)
calculateSparseHash ls =
  foldl' (\acc _ -> runRound acc ls) initialConfig [(1 :: Int)..64]

-- Stolen from https://stackoverflow.com/a/12882583/126014
chunksOf :: Int -> [a] -> [[a]]
chunksOf n =
  Prelude.takeWhile (not . null) . Data.List.unfoldr (Just . Prelude.splitAt n)

calculateDenseHash :: (Num b, Bits b) => [b] -> [b]
calculateDenseHash = fmap (foldl' xor 0) . chunksOf 16

asHex :: [Int] -> String
asHex = concatMap (printf "%02x")

calculateKnotHash :: String -> String
calculateKnotHash inp =
  asHex
  $ calculateDenseHash
  $ fcData
  $ snd
  $ calculateSparseHash
  $ lengthsFromAscii inp ++ standardSuffix
