{-# LANGUAGE DeriveFunctor #-}
module Day10 where

import Data.Bits (Bits, xor)
import Data.Bifunctor (first)
import Data.List.NonEmpty as NonEmpty hiding (xor)
import Data.List (unfoldr)
import Text.Printf (printf)

-- Comonad

class Functor w => Comonad w where
  extract :: w a -> a
  duplicate :: w a -> w (w a)
  duplicate = extend id
  extend :: (w a -> b) -> w a -> w b
  extend f = fmap f . duplicate

-- Focused cycle
-- Favoured the term 'cycle' over 'ring', in order to avoid confusion with
-- algebraic structures like rings, semirings, and so on.

data FocusedCycle a =
  FocusedCycle { fcPosition :: Int, fcData :: NonEmpty a } deriving (Show, Eq, Functor)

normalizeIndex :: NonEmpty a -> Int -> Int
normalizeIndex l idx = idx `mod` NonEmpty.length l

instance Comonad FocusedCycle where
  extract (FocusedCycle p d) = d NonEmpty.!! p
  duplicate (FocusedCycle p d) = FocusedCycle p $ fmap (`FocusedCycle` d) indices
    where
      -- Zipping an infinite list of indices with the real data list `d` ensures
      -- that the length of the indices list is correct, and retains it as a
      -- NonEmpty list without any unsafe operations.
      indices = fst <$> NonEmpty.zip (fmap (normalizeIndex d) $ p :| [p + 1 ..]) d

moveFocus :: Int -> FocusedCycle a -> FocusedCycle a
moveFocus n fc@(FocusedCycle p d) = fc { fcPosition = normalizeIndex d $ p + n }

-- Puzzle 1

step :: (Int, FocusedCycle a) -> Int -> (Int, FocusedCycle a)
step (skipSize, fc) l =
  let -- Enumerate list from focus, possibly wrapping around, then split into
      -- two lists, the first of which has the length `l`. Finally, reverse the
      -- first of these two lists.
      (twisted, rest) =
        first Prelude.reverse $ NonEmpty.splitAt l $ fcData $ extend extract fc
      -- Get indices for the current position. If, for instance, fcPosition is 3
      -- and the list is 5 elements long, the indices are as follows:
      -- λ> fcData $ fcPosition <$> duplicate (FocusedCycle 3 $ 7 :| [6,8,5,9])
      -- 3 :| [4,0,1,2]
      -- This is because the first index (at focus) is 3, the next is 4, but
      -- then the end of the list has been reached, so wrapping around, the next
      -- indices are 0, 1, and 2.
      indices = fcData $ fcPosition <$> duplicate fc
      -- Use the above indices to reorder the twisted and residual list so that
      -- the new list starts at the right index.
      newData =
        fmap snd
        $ NonEmpty.sortWith fst
        $ NonEmpty.zip indices (NonEmpty.fromList $ twisted ++ rest)
      -- Calculation is done. Move the focus.
      fc' = moveFocus (skipSize + l) fc
  in (skipSize + 1, fc' { fcData = newData })

runRound :: Foldable t => (Int, FocusedCycle a) -> t Int -> (Int, FocusedCycle a)
runRound = Prelude.foldl step

initialConfig :: (Int, FocusedCycle Int)
initialConfig = (0, FocusedCycle 0 $ 0 :| [1..255])

solve1 :: Foldable t => t Int -> Int
solve1 = product . NonEmpty.take 2 . fcData . snd . runRound initialConfig

-- Puzzle 2

lengthsFromAscii :: String -> [Int]
lengthsFromAscii = fmap fromEnum

standardSuffix :: [Int]
standardSuffix = [17, 31, 73, 47, 23]

-- Slow :(
calculateSparseHash :: Foldable t => t Int -> (Int, FocusedCycle Int)
calculateSparseHash ls = imp initialConfig (1 :: Int)
  where
    imp x 64 = runRound x ls
    imp x  i = imp (runRound x ls) (i + 1)

-- Stolen from https://stackoverflow.com/a/12882583/126014
chunksOf :: Int -> [a] -> [[a]]
chunksOf n =
  Prelude.takeWhile (not . null) . Data.List.unfoldr (Just . Prelude.splitAt n)

calculateDenseHash :: (Num b, Bits b) => [b] -> [b]
calculateDenseHash = fmap (foldl xor 0) . chunksOf 16

asHex :: [Int] -> String
asHex = concatMap (printf "%02x")

solve2 :: String -> String
solve2 inp =
  asHex
  $ calculateDenseHash
  $ NonEmpty.toList
  $ fcData
  $ snd
  $ calculateSparseHash
  $ lengthsFromAscii inp ++ standardSuffix

-- Tests
test :: Bool
test = and
  [
    (1, FocusedCycle 3 $ (2 :: Int) :| [1, 0, 3, 4]) == step (0, FocusedCycle 0 $ 0 :| [1..4]) 3,
    (2, FocusedCycle 3 $ (4 :: Int) :| [3, 0, 1, 2]) == step (1, FocusedCycle 3 $ 2 :| [1, 0, 3, 4]) 4,
    (3, FocusedCycle 1 $ (4 :: Int) :| [3, 0, 1, 2]) == step (2, FocusedCycle 3 $ 4 :| [3, 0, 1, 2]) 1,
    (4, FocusedCycle 4 $ (3 :: Int) :| [4, 2, 1, 0]) == step (3, FocusedCycle 1 $ 4 :| [3, 0, 1, 2]) 5,
    (12 :: Int) == product (NonEmpty.take 2 $ fcData $ snd $ runRound (0, FocusedCycle 0 $ 0 :| [1..4]) [3, 4, 1, 5]),
    (11413 :: Int) == solve1 [106,16,254,226,55,2,1,166,177,247,93,0,255,228,60,36] -- My input
  ]
