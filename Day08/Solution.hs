module Day08 where

import Data.Maybe (fromMaybe)
import Text.ParserCombinators.ReadP
import qualified Data.Map.Strict as Map

data Inst = INC | DEC deriving (Show, Eq)
data Op = EQL | NEQ | LTh | LTE | GTh | GTE deriving (Show, Eq)

-- Parsing

parseRegister :: ReadP String
parseRegister = munch (`elem` ['a'..'z']) <* skipSpaces

parseInst :: ReadP Inst
parseInst =
  (string "inc" *> pure INC) +++ (string "dec" *> pure DEC) <* skipSpaces

parseNumber :: ReadP Int
parseNumber =
  fmap read
  $ pure (++)
  <*> option "" (string "-")
  <*> munch (`elem` ['0'..'9'])
  <* skipSpaces

parseOp :: ReadP Op
parseOp = choice
  [
    string "==" *> pure EQL,
    string "!=" *> pure NEQ,
    string "<"  *> pure LTh,
    string "<=" *> pure LTE,
    string ">"  *> pure GTh,
    string ">=" *> pure GTE
  ]
  <* skipSpaces

parseLine :: ReadS (String, Inst, Int, String, Op, Int)
parseLine =
  readP_to_S
  $ pure (\reg1 inst i1 reg2 op i2 -> (reg1, inst, i1, reg2, op, i2))
  <*> parseRegister
  <*> parseInst
  <*> parseNumber
  <*  string "if"
  <*  skipSpaces
  <*> parseRegister
  <*> parseOp
  <*> parseNumber
  <*  eof

parseInput :: String -> [(String, Inst, Int, String, Op, Int)]
parseInput =
  concatMap (fmap fst . filter ((== "") . snd)) . fmap parseLine . lines

-- Puzzle 1

evalCondImp :: (Num a, Ord k) => k -> (a -> b -> c) -> b -> Map.Map k a -> c
evalCondImp reg op x registers = Map.findWithDefault 0 reg registers `op` x

evalCond :: (Ord a, Ord k, Num a) => (b, c, d, k, Op, a) -> Map.Map k a -> Bool
evalCond (_, _, _, reg, EQL, x) = evalCondImp reg (==) x
evalCond (_, _, _, reg, NEQ, x) = evalCondImp reg (/=) x
evalCond (_, _, _, reg, LTh, x) = evalCondImp reg  (<) x
evalCond (_, _, _, reg, LTE, x) = evalCondImp reg (<=) x
evalCond (_, _, _, reg, GTh, x) = evalCondImp reg  (>) x
evalCond (_, _, _, reg, GTE, x) = evalCondImp reg (>=) x

performInstruction :: (Num a, Ord k) => (k, Inst, a, b, c, d) -> Map.Map k a -> Map.Map k a
performInstruction (reg, INC, x, _, _, _) = Map.alter (pure .      (+ x) . fromMaybe 0) reg
performInstruction (reg, DEC, x, _, _, _) = Map.alter (pure . subtract x . fromMaybe 0) reg

evalLine :: (Num a, Ord k, Ord a) => (k, Inst, a, k, Op, a) -> Map.Map k a -> Map.Map k a
evalLine line registers =
  if evalCond line registers
    then performInstruction line registers
    else registers

solve1 :: String -> Int
solve1 =
  maximum . fmap snd . Map.toList . foldl (flip evalLine) Map.empty . parseInput

-- Puzzle 2

solve2 :: String -> Int
solve2 =
  maximum . fmap snd . concatMap Map.toList . scanl (flip evalLine) Map.empty . parseInput

-- Tests

test :: IO Bool
test = do
  inp <- readFile "./example.txt"
  return $ and
    [
        1 == solve1 inp,
       10 == solve2 inp
    ]
