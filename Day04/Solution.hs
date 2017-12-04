module Day04 where

import Data.List (nub, permutations)

solve1 :: String -> Int
solve1 =
  length . filter (\ws -> length (nub ws) == length ws) . fmap words . lines

areAnagrams :: Eq a => [a] -> [a] -> Bool
areAnagrams x y = x `elem` permutations y

containsAnagrams :: Eq a => [[a]] -> Bool
containsAnagrams [] = False
containsAnagrams (x:xs) = any (areAnagrams x) xs || containsAnagrams xs

solve2 :: String -> Int
solve2 = length . filter (not . containsAnagrams) . fmap words . lines
