module Day23 where

import Data.List (genericIndex, genericLength, unfoldr)
import Data.Maybe (fromMaybe, listToMaybe)
import Text.ParserCombinators.ReadP
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data Contents = Value Integer | Register Char deriving (Show, Eq)
data Instruction =
    Set Char Contents
  | Sub Char Contents
  | Mul Char Contents
  | Jnz Contents Contents
  deriving (Show, Eq)

-- Parsing

parseRegister :: ReadP Char
parseRegister = choice (fmap char ['a'..'z']) <* skipSpaces

parseNumber :: ReadP Integer
parseNumber =
  fmap read
  $ pure (++)
  <*> option "" (string "-")
  <*> munch (`elem` ['0'..'9'])
  <*  skipSpaces

parseContents :: ReadP Contents
parseContents = fmap Register parseRegister <++ fmap Value parseNumber

parseInstruction :: ReadP Instruction
parseInstruction = choice
  [
    Set <$> (string "set" <* skipSpaces *> parseRegister) <*> parseContents,
    Sub <$> (string "sub" <* skipSpaces *> parseRegister) <*> parseContents,
    Mul <$> (string "mul" <* skipSpaces *> parseRegister) <*> parseContents,
    Jnz <$> (string "jnz" <* skipSpaces *> parseContents) <*> parseContents
  ]

parseLine :: ReadS Instruction
parseLine = readP_to_S (parseInstruction <* eof)

parseInput :: String -> [Instruction]
parseInput =
  concatMap (fmap fst . filter ((== "") . snd)) . fmap parseLine . lines

-- Puzzle 1

data ProgramState = ProgramState
  { registers :: Map Char Integer
  , position :: Integer }
  deriving (Show, Eq)

getRegisterValue :: Contents -> ProgramState -> Integer
getRegisterValue    (Value i)  _ = i
getRegisterValue (Register r) ps = Map.findWithDefault 0 r $ registers ps

move :: Integer -> ProgramState -> ProgramState
move i ps = ps { position = position ps + i }

coprocOp :: (Integer -> Integer -> Integer) -> Char -> Contents
          -> ProgramState -> ProgramState
coprocOp op register contents ps =
  let rv = getRegisterValue contents ps
      updatedRegisters =
        Map.alter (Just . op rv . fromMaybe 0) register $ registers ps
  in move 1 $ ps { registers = updatedRegisters }

coprocSet, coprocSub, coprocMul :: Char -> Contents -> ProgramState -> ProgramState
coprocSet = coprocOp const
coprocSub = coprocOp $ flip (-)
coprocMul = coprocOp (*)

coprocJnz :: Contents -> Contents -> ProgramState -> ProgramState
coprocJnz x y ps =
  let xValue = getRegisterValue x ps
      yValue = getRegisterValue y ps
  in if xValue == 0 then move 1 ps else move yValue ps

evalInstruction :: Instruction -> ProgramState -> ProgramState
evalInstruction (Set x y) = coprocSet x y
evalInstruction (Sub x y) = coprocSub x y
evalInstruction (Mul x y) = coprocMul x y
evalInstruction (Jnz x y) = coprocJnz x y

currentInstruction :: [a] -> ProgramState -> a
currentInstruction prg ps = prg `genericIndex` position ps

step :: [Instruction] -> ProgramState -> Maybe ProgramState
step prg ps =
  let pos = position ps
      l = genericLength prg
  in if 0 <= pos && pos < l
    then Just $ evalInstruction (currentInstruction prg ps) ps
    else Nothing

solve1 :: String -> Int
solve1 inp = length $ filter isMul $ unfoldr imp $ ProgramState Map.empty 0
  where
    prg = parseInput inp
    imp ps = do
      let inst = currentInstruction prg ps
      ps' <- step prg ps
      return (inst, ps')
    isMul (Mul _ _) = True
    isMul        _  = False

-- Puzzle 2

-- I don't know if a programmatic solution to puzzle 2 exists, but I was able to
-- reason most of it out by 'reading the code', so to speak. I took my puzzle
-- input and annotated it with its jump instructions, like this:
--
-- set b 65
-- set c b
-- jnz a 2 ----+
--             |
-- jnz 1 5 -------+
--             |  |
-- mul b 100 <-+  |
-- sub b -100000  |
-- set c b        |
-- sub c -17000   |
--                |
-- set f 1 <------+<---+
-- set d 2             |
--                     |
-- set e 2 <--------+  |
--                  |  |
-- set g d <-----+  |  |
-- mul g e       |  |  |
-- sub g b       |  |  |
-- jnz g 2 ---+  |  |  |
--            |  |  |  |
-- set f 0    |  |  |  |
--            |  |  |  |
-- sub e -1 <-+  |  |  |
-- set g e       |  |  |
-- sub g b       |  |  |
-- jnz g -8 -----+  |  |
--                  |  |
-- sub d -1         |  |
-- set g d          |  |
-- sub g b          |  |
-- jnz g -13 -------+  |
--                     |
-- jnz f 2 --+         |
--           |         |
-- sub h -1  |         |
--           |         |
-- set g b <-+         |
-- sub g c             |
-- jnz g 2 ----+       |
--             |       |
-- jnz 1 3 -------+    |
--             |  |    |
-- sub b -17 <-+  |    |
-- jnz 1 -23 ----------+
--                |
--            Terminate
--
-- I then arrived at a couple of observations:
--
-- 1. The first part of the program is initialization code. Until the `set f 1`
-- instruction, no loops occur. The initialization part of the program
-- initializes registers 'b' and 'c'; if 'a' is set, these two registers get
-- much larger values than when 'a' isn't set.
--
-- 2. There are three nested loops. The inner loop increments 'e' until it
-- reaches 'b'; the middle loop increments 'd' until it reaches 'b', and the
-- outer loop increments 'b' with 17 until it reaches 'c'. The difference
-- between 'c' and 'd' is exactly 17,000 when 'a' is set, so the outer loop
-- executes a 1,000 times.
--
-- 3. In each loop, 'f' is set when 'g' is 0. The innermost loop multiplies 'd'
-- and 'e' until both reach 'b'. 'g' only becomes 0 when the product of 'd' and
-- 'e' is exactly the same as 'b'. Conversely, when 'b' is prime number, this
-- will not happen, and 'f' will not be set. In other words, 'f' is only set
-- when 'b' isn't a prime. I admit that I had to get a hint for this particular
-- step...
--
-- 4. 'h' is only incremented when 'f' is 0. The number of times that happens is
-- the number of times that the number sequence of 'b' doesn't contain a prime.
-- That number sequence starts at 106,500 (65 * 100 - (-100,000)), increments
-- by 17, and stops at 123,500 (65 * 100 - (-100,000) - (-17,000)).

-- Stolen from https://wiki.haskell.org/Testing_primality
factors :: Integer -> [Integer]
factors = unfoldr (\n -> listToMaybe [(x, div n x) | x <- [2..n], mod n x == 0])

isPrime :: Integer -> Bool
isPrime n = n > 1 && head (factors n) == n

solve2 :: Int
solve2 = length $ filter (not . isPrime) [106500, 106500 + 17 .. 123500]
