module Day03 where

-- Not at all sure about this implementation. I had several false starts, so I'm
-- afraid that I came at this puzzle from the wrong direction.
-- I wouldn't be surprised if a much more elegant solution exists, using a
-- radically different approach, but obviously, it eluded me.

import Data.List (find, unfoldr)

-- Zero-indexed size of each ring side. Ring 0 has size 1, ring 1 has size 3,
-- ring 2 has size 5, and so on...
ringSize :: Num a => a -> a
ringSize n = n * 2 + 1

-- Enumerates all the coordinates in a ring
ringCoordinates :: Integral a => a -> [(a, a)]
ringCoordinates r =
  let halfSide = ringSize r `div` 2
      -- Produces a list like [-2, -1, 0, 1, 2, 3] (for ring size 7)
      vertical = [(-halfSide + 1) .. halfSide]
      -- Produces a list like [2, 1, 0, -1, -2, -3] (for ring size 7)
      horizontal = negate <$> vertical
  in [( r,  i) | i <- vertical] ++
     [( i,  r) | i <- horizontal] ++
     [(-r, -i) | i <- vertical] ++
     [(-i, -r) | i <- horizontal]

-- Enumerates all coordinates, starting at (0, 0) and spiralling out from there.
coordinates :: [(Integer, Integer)]
coordinates = (0, 0) : concatMap ringCoordinates [1..]

solve1 :: Int -> Integer
solve1 val =
  let (x, y) = coordinates !! (val - 1)
  in abs x + abs y

neighbors :: Num a => (a, a) -> [(a, a)]
neighbors (x, y) =
  [
    (x - 1, y + 1), (x, y + 1), (x + 1, y + 1),
    (x - 1, y    ),             (x + 1, y    ),
    (x - 1, y - 1), (x, y - 1), (x + 1, y - 1)
  ]

calculateNext :: Num a => [a] -> a
calculateNext xs =
  let plotted = zip coordinates xs
      nextCoordinate = coordinates !! length xs
      candidates = neighbors nextCoordinate
  in sum $ snd <$> filter (\(coord, _) -> coord `elem` candidates) plotted

-- Enumerates all sums calculated by the neighbors already produced.
-- Horribly inefficient!
neighborSums :: [Integer]
neighborSums = unfoldr (\xs -> Just (last xs, xs ++ [calculateNext xs])) [1]

solve2 :: Integer -> Maybe Integer
solve2 target = find (> target) neighborSums

test :: Bool
test = and
  [
      3 == solve1 12,
      2 == solve1 23,
     31 == solve1 1024,
    419 == solve1 289326, -- my puzzle input
    [1, 1, 2, 4, 5, 10, 11, 23, 25, 26, 54, 57, 59, 122] == take 14 neighborSums,
    Just 295229 == solve2 289326 -- my puzzle input
  ]
