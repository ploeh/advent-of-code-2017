{-# LANGUAGE BinaryLiterals #-}
module Day15 where

import Data.Bits (Bits, (.&.))
import Data.List (unfoldr)

-- Puzzle 1

denominator :: Integer
denominator = 2147483647

generate1 :: Integer -> Integer -> [Integer]
generate1 factor = unfoldr (\prev -> Just (calc prev, calc prev))
  where calc x = (x * factor) `rem` denominator

generate1A, generate1B :: Integer -> [Integer]
generate1A = generate1 16807
generate1B = generate1 48271

lowest16Bits :: (Num a, Bits a) => a -> a
lowest16Bits i = i .&. 0b1111111111111111

judge :: (Bits a, Num a) => ([a], [a]) -> Int -> Int
judge (ga, gb) l =
  length
  $ filter (uncurry (==))
  $ take l
  $ zip (lowest16Bits <$> ga) (lowest16Bits <$> gb)

solve1 :: (Integer, Integer) -> Int
solve1 (seedA, seedB) = judge (generate1A seedA, generate1B seedB) 40000000

-- Puzzle 2

isMultipleOf :: Integral a => a -> a -> Bool
isMultipleOf divisor i = 0 == i `rem` divisor

generate2A, generate2B :: Integer -> [Integer]
generate2A = filter (isMultipleOf 4) . generate1A
generate2B = filter (isMultipleOf 8) . generate1B

solve2 :: (Integer, Integer) -> Int
solve2 (seedA, seedB) = judge (generate2A seedA, generate2B seedB) 5000000

-- Tests

test :: Bool
test = and
  [
    [   1092455, 1181022009,  245556042, 1744312007, 1352636452] == take 5 (generate1A   65),
    [ 430625591, 1233683848, 1431495498,  137874439,  285222916] == take 5 (generate1B 8921),
    [1352636452, 1992081072,  530830436, 1980017072,  740335192] == take 5 (generate2A   65),
    [1233683848,  862516352, 1159784568, 1616057672,  412269392] == take 5 (generate2B 8921),
    1 == judge (generate1A 65, generate1B 8921)    5,
    1 == judge (generate2A 65, generate2B 8921) 1056
  ]
