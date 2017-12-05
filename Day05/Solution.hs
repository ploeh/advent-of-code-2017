module Day05 where

import Data.Sequence (Seq, index, adjust, fromList, length)

data JumpList = JumpList { jlIndex :: Int, jlList :: Seq Int } deriving (Show, Eq)

move :: (Int -> Int) -> JumpList -> JumpList
move f (JumpList idx instructions) =
  let instr = index instructions idx
      newInstructions = adjust f idx instructions
  in JumpList (idx + instr) newInstructions

hasEscaped :: JumpList -> Bool
hasEscaped (JumpList idx instructions) =
  idx >= Data.Sequence.length instructions

parseJumpList :: String -> JumpList
parseJumpList = JumpList 0 . fromList . fmap read . words

solve1 :: JumpList -> Int
solve1 = Prelude.length . takeWhile (not . hasEscaped) . iterate (move (+ 1))

adj2 :: (Num a, Ord a) => a -> a
adj2 x = if x >= 3 then x - 1 else x + 1

-- Takes a ridiculously long time to run for my real input!
solve2 :: JumpList -> Int
solve2 = Prelude.length . takeWhile (not . hasEscaped) . iterate (move adj2)

test :: Bool
test = and
  [
     5 == solve1 (JumpList 0 $ fromList [0, 3, 0, 1, -3]),
    10 == solve2 (JumpList 0 $ fromList [0, 3, 0, 1, -3]),
    fromList [2, 3, 2, 3, -1] == jlList (iterate (move adj2) (JumpList 0 $ fromList [0, 3, 0, 1, -3]) !! 10)
  ]
