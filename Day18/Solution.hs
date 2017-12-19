module Day18 where

import Data.Bifunctor (first, second)
import Data.Maybe (fromMaybe)
import Data.Tuple (swap)
import Data.List (unfoldr, genericLength, genericIndex)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Lazy (StateT, runStateT, gets, modify)
import Text.ParserCombinators.ReadP
import qualified Data.Map.Strict as Map

data Contents = Value Integer | Register Char deriving (Show, Eq)
data Instruction =
    Snd Contents
  | Set Char Contents
  | Add Char Contents
  | Mul Char Contents
  | Mod Char Contents
  | Rcv Char
  | Jgz Contents Contents
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
    Snd <$> (string "snd" <* skipSpaces *> parseContents),
    Set <$> (string "set" <* skipSpaces *> parseRegister) <*> parseContents,
    Add <$> (string "add" <* skipSpaces *> parseRegister) <*> parseContents,
    Mul <$> (string "mul" <* skipSpaces *> parseRegister) <*> parseContents,
    Mod <$> (string "mod" <* skipSpaces *> parseRegister) <*> parseContents,
    Rcv <$> (string "rcv" <* skipSpaces *> parseRegister),
    Jgz <$> (string "jgz" <* skipSpaces *> parseContents) <*> parseContents
  ]

parseLine :: ReadS Instruction
parseLine = readP_to_S (parseInstruction <* eof)

parseInput :: String -> [Instruction]
parseInput =
  concatMap (fmap fst . filter ((== "") . snd)) . fmap parseLine . lines

-- Puzzle 1

data ProgramState = ProgramState
  { registers :: Map.Map Char Integer
  , sendQueue :: ![Integer]
  , receiveQueue :: ![Integer]
  , program :: [Instruction]
  , position :: Integer }
  deriving (Show, Eq)

getRegisterValue :: Contents -> ProgramState -> Integer
getRegisterValue    (Value i)  _ = i
getRegisterValue (Register r) ps = Map.findWithDefault 0 r $ registers ps

move :: Integer -> ProgramState -> ProgramState
move i ps = ps { position = position ps + i }

duetSnd :: Contents -> ProgramState -> ProgramState
duetSnd contents ps =
  let rv = getRegisterValue contents ps
  in move 1 $ ps { sendQueue = rv : sendQueue ps }

duetOp :: (Integer -> Integer -> Integer) -> Char -> Contents
          -> ProgramState -> ProgramState
duetOp op register contents ps =
  let rv = getRegisterValue contents ps
      updatedRegisters =
        Map.alter (Just . op rv . fromMaybe 0) register $ registers ps
  in move 1 $ ps { registers = updatedRegisters }

duetSet, duetAdd, duetMul, duetMod :: Char -> Contents
                                      -> ProgramState -> ProgramState
duetSet = duetOp const
duetAdd = duetOp (+)
duetMul = duetOp (*)
duetMod = duetOp $ flip mod

duetRcv :: Char -> ProgramState -> Maybe ProgramState
duetRcv register ps =
  case receiveQueue ps of
    [] -> Nothing
    (msg:msgs) ->
      Just $ duetSet register (Value msg) $ ps { receiveQueue = msgs }

duetJgz :: Contents -> Contents -> ProgramState -> ProgramState
duetJgz x y ps =
  let xValue = getRegisterValue x ps
      yValue = getRegisterValue y ps
  in if xValue <= 0 then move 1 ps else move yValue ps

evalInstruction :: Instruction -> ProgramState -> Maybe ProgramState
evalInstruction (Snd x)   = Just . duetSnd x
evalInstruction (Set x y) = Just . duetSet x y
evalInstruction (Add x y) = Just . duetAdd x y
evalInstruction (Mul x y) = Just . duetMul x y
evalInstruction (Mod x y) = Just . duetMod x y
evalInstruction (Rcv x)   =        duetRcv x
evalInstruction (Jgz x y) = Just . duetJgz x y

initializeProgram :: [Instruction] -> ProgramState
initializeProgram instructions = ProgramState Map.empty [] [] instructions 0

step :: ProgramState -> Maybe ProgramState
step ps =
  let pos = position ps
      prg = program ps
      l = genericLength prg
      currentInstruction = prg `genericIndex` pos
  in if 0 <= pos && pos < l
    then evalInstruction currentInstruction ps
    else Nothing

-- Run a program until it halts. This can happen if the program position falls
-- off one of the ends (less than zero, or beyond the last line of the program),
-- or when it reaches a `rcv` instruction and the receive queue is empty.
run :: ProgramState -> [ProgramState]
run = unfoldr (fmap (\x -> (x, x)) . step)

solve1 :: String -> Integer
solve1 = head . sendQueue . last . run . initializeProgram . parseInput

-- Puzzle 2

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast xs = Just $ last xs

-- This function returns the two programs, as well as a counter. The counter
-- will keep track of the number of sends program 1 performed. Not the most
-- general solution one could imagine, but I admit that I (due to failures
-- entirely of my own making) already struggled enough with this puzzle that I
-- don't feel motivated to polish this into a better, more abstract model.
initializeDuet :: Num a => String -> (a, (ProgramState, ProgramState))
initializeDuet inp =
  let ps = initializeProgram $ parseInput inp
      p0 = ps { registers = Map.insert 'p' 0 $ registers ps }
      p1 = ps { registers = Map.insert 'p' 1 $ registers ps }
  in (0, (p0, p1))

-- Run each of the programs once, starting with x and then y. First, x runs
-- until it halts. Then x's send queue is appended to y's receive queue
-- (reversed, because a list normally works as a stack instead of a queue).
-- Subsequently, y runs until it halts. Finally, y's send queue is transferred
-- to x in the same manner that x's send queue was transferred to y earlier.
-- Since both programs' send queues have been transferred to the other program,
-- each of the programs' send queues are bound to an empty list.
-- The counter is updated with the length of program 1's send queue, because
-- that was as many times that program sent a value during its time of activity.
runEachOnce :: (Int, (ProgramState, ProgramState))
               -> Maybe (Int, (ProgramState, ProgramState))
runEachOnce (counter, (x, y)) = (runStateT $ runEachOnce' counter) (x, y)

runEachOnce' :: Int -> StateT (ProgramState, ProgramState) Maybe Int
runEachOnce' counter = do
  run0
  modify transferMessages
  run1
  sentBy1 <- gets (length . sendQueue . snd)
  modify (swap . transferMessages . swap)
  return $ counter + sentBy1

transferMessages :: (ProgramState, ProgramState) -> (ProgramState, ProgramState)
transferMessages (x, y) =
  let sq = sendQueue x
      rq = receiveQueue y
  in (x { sendQueue = [] }, y { receiveQueue = rq ++ reverse sq })

run0 :: StateT (ProgramState, ProgramState) Maybe ()
run0 = do
  p0  <- gets fst
  p0' <- lift $ safeLast $ run p0
  modify (first (const p0'))

run1 :: StateT (ProgramState, ProgramState) Maybe ()
run1 = do
  p1  <- gets snd
  p1' <- lift $ safeLast $ run p1
  modify (second (const p1'))

runDuo :: (Int, (ProgramState, ProgramState))
          -> (Int, (ProgramState, ProgramState))
runDuo duo =
  case runEachOnce duo of
    Just duo' -> runDuo duo'
    Nothing   -> duo

solve2 :: String -> Int
solve2 = fst . runDuo . initializeDuet

-- Tests

example1, example2 :: String
example1 = "set a 1\nadd a 2\nmul a a\nmod a 5\nsnd a\nset a 0\nrcv a\njgz a -1\nset a 1\njgz a -2"
example2 = "snd 1\nsnd 2\nsnd p\nrcv a\nrcv b\nrcv c\nrcv d"

test :: Bool
test = and
  [
    4 == solve1 example1,
    3 == solve2 example2
  ]
