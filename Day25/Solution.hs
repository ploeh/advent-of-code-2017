module Day25 where

import Data.List (find, unfoldr)
import Data.Set (Set)
import qualified Data.Set as Set
import Prelude hiding (Left, Right)

data TMachine = TMachine
  { cursorPosition :: Integer
  , currentState :: Char
  , tape :: Set Integer }
  deriving (Show, Eq)

initialMachine :: Char -> TMachine
initialMachine s = TMachine 0 s Set.empty

data Value = Zero | One deriving (Show, Eq)

data Direction = Left | Right deriving (Show, Eq)

data Instruction = Instruction
  { targetState :: Char
  , targetValue :: Value
  , writeValue :: Value
  , direction :: Direction
  , nextState :: Char }
  deriving (Show, Eq)

isMatch :: Char -> Value -> Instruction -> Bool
isMatch ts tv (Instruction is iv _ _ _) = ts == is && tv == iv

step :: Foldable t => t Instruction -> TMachine -> Maybe TMachine
step prg machine = do
  let ts = currentState machine
  let pos = cursorPosition machine
  let tv = if Set.member pos $ tape machine then One else Zero
  instruction <- find (isMatch ts tv) prg
  let wv = writeValue instruction
  let tape' =
        case wv of
          Zero -> Set.delete pos $ tape machine
          One  -> Set.insert pos $ tape machine
  let pos' = if direction instruction == Left then pos - 1 else pos + 1
  return $ TMachine pos' (nextState instruction) tape'

run :: Foldable t => t Instruction -> Char -> [TMachine]
run prg s =
  let step' = step prg
  in unfoldr (fmap (\x -> (x, x)) . step') $ initialMachine s

-- Puzzle 1

-- While I could have written a parser for the instruction 'language' in the
-- puzzle, it was faster to just translate the instuctions into the following
-- instruction set.
program1 :: [Instruction]
program1 =
  [
    Instruction 'A' Zero  One Right 'B',
    Instruction 'A'  One Zero  Left 'C',
    Instruction 'B' Zero  One  Left 'A',
    Instruction 'B'  One  One Right 'D',
    Instruction 'C' Zero  One Right 'A',
    Instruction 'C'  One Zero  Left 'E',
    Instruction 'D' Zero  One Right 'A',
    Instruction 'D'  One Zero Right 'B',
    Instruction 'E' Zero  One  Left 'F',
    Instruction 'E'  One  One  Left 'C',
    Instruction 'F' Zero  One Right 'D',
    Instruction 'F'  One  One Right 'A'
  ]

-- Takes some 2-3 minutes to run on my machine...
solve1 :: Int
solve1 = Set.size $ tape $ run program1 'A' !! (12919244 - 1)

-- Tests

example1 :: [Instruction]
example1 =
  [
    Instruction 'A' Zero  One Right 'B',
    Instruction 'A'  One Zero  Left 'B',
    Instruction 'B' Zero  One  Left 'A',
    Instruction 'B'  One  One Right 'A'
  ]

test :: Bool
test = 3 == Set.size (tape $ run example1 'A' !! 5)
