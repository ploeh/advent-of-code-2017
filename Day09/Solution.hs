module Day09 where

-- Puzzle 1

skipGarbage :: Num a => a -> a -> String -> a
skipGarbage nest score ('!':_:cs) = skipGarbage nest score cs
skipGarbage nest score   ('>':cs) = scoreGroups nest score cs
skipGarbage nest score     (_:cs) = skipGarbage nest score cs
skipGarbage    _ score         [] = score

scoreGroups :: Num a => a -> a -> String -> a
scoreGroups nest score ('{':cs) = scoreGroups (nest + 1) (score + nest + 1) cs
scoreGroups nest score ('}':cs) = scoreGroups (nest - 1)             score  cs
scoreGroups nest score ('<':cs) = skipGarbage      nest              score  cs
scoreGroups nest score   (_:cs) = scoreGroups      nest              score  cs
scoreGroups    _ score       [] = score

solve1 :: String -> Integer
solve1 = scoreGroups 0 0

-- Puzzle 2

countGarbage :: Num a => a -> String -> a
countGarbage count ('!':_:cs) = countGarbage count cs
countGarbage count   ('>':cs) =    skipGroup count cs
countGarbage count     (_:cs) = countGarbage (count + 1) cs
countGarbage count [] = count

skipGroup :: Num a => a -> String -> a
skipGroup count ('<':cs) = countGarbage count cs
skipGroup count   (_:cs) = skipGroup count cs
skipGroup count [] = count

solve2 :: String -> Integer
solve2 = skipGroup 0

-- Tests

test :: Bool
test = and
  [
     1 == solve1 "{}",
     6 == solve1 "{{{}}}",
     5 == solve1 "{{},{}}",
    16 == solve1 "{{{},{},{{}}}}",
     1 == solve1 "{<a>,<a>,<a>,<a>}",
     9 == solve1 "{{<ab>},{<ab>},{<ab>},{<ab>}}",
     9 == solve1 "{{<!!>},{<!!>},{<!!>},{<!!>}}",
     3 == solve1 "{{<a!>},{<a!>},{<a!>},{<ab>}}",
     0 == solve2 "<>",
    17 == solve2 "<random characters>",
     3 == solve2 "<<<<>",
     2 == solve2 "<{!>}>",
     0 == solve2 "<!!>",
     0 == solve2 "<!!!>>",
    10 == solve2 "<{o\"i!a,<{i<a>"
  ]
