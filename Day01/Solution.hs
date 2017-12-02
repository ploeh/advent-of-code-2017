module Day01 where

import Data.Char (digitToInt)

digitize :: Functor f => f Char -> f Int
digitize = fmap digitToInt

prep1 :: [b] -> [(b, b)]
prep1 (x:xs) = zip (x:xs) (xs ++ [x])
prep1 [] = []

prep2 :: [b] -> [(b, b)]
prep2 xs =
  let xs' = xs ++ xs
      l   = length xs
      fwd = div l 2
  in zip xs (take l $ drop fwd xs')

solve :: (Functor f, Eq a, Num a) => (f Int -> [(a, a)]) -> f Char -> a
solve prep = sum . fmap fst . filter (uncurry (==)) . prep . digitize

solve1 :: String -> Int
solve1 = solve prep1

solve2 :: String -> Int
solve2 = solve prep2

test :: Bool
test = and
  [
     3 == solve1 "1122",
     4 == solve1 "1111",
     0 == solve1 "1234",
     9 == solve1 "91212129",
     6 == solve2 "1212",
     0 == solve2 "1221",
     4 == solve2 "123425",
    12 == solve2 "123123",
     4 == solve2 "12131415"
  ]
