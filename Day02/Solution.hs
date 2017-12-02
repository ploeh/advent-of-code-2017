module Day02 where

lineDiff :: (Read a, Ord a, Num a) => String -> a
lineDiff s =
  let l = read <$> words s
  in maximum l - minimum l

solve1 :: (Ord a, Read a, Num a) => String -> a
solve1 s = sum $ lineDiff <$> lines s

lineDiv :: (Read a, Integral a) => String -> [a]
lineDiv s =
  let l = read <$> words s
  in fmap fst $ filter (\(q, r) -> q /= 1 && r == 0) $ concatMap (\x -> fmap (divMod x) l) l

solve2 :: (Integral a, Read a) => String -> a
solve2 s = sum $ concatMap lineDiv $ lines s
