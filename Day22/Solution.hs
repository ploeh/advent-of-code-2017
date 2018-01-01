{-# LANGUAGE NumDecimals #-}
module Day22 where

import Data.List (unfoldr)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Prelude hiding (Right, Left)

-- Model

data Direction = Up | Left | Down | Right deriving (Show, Eq)

data Node = Node Integer Integer deriving (Show, Eq, Ord)

data InfectionState a = InfectionState
  { carrierPosition :: Node
  , carrierDirection :: Direction
  , nodes :: Map Node a }
  deriving (Show, Eq)

turnRight :: Direction -> Direction
turnRight    Up = Right
turnRight Right =  Down
turnRight  Down =  Left
turnRight  Left =    Up

turnLeft :: Direction -> Direction
turnLeft     Up =  Left
turnLeft   Left =  Down
turnLeft   Down = Right
turnLeft  Right =    Up

reverseDirection :: Direction -> Direction
reverseDirection    Up =  Down
reverseDirection  Left = Right
reverseDirection  Down =    Up
reverseDirection Right =  Left

move :: Direction -> Node -> Node
move    Up (Node x y) = Node  x     (y + 1)
move  Left (Node x y) = Node (x - 1) y
move  Down (Node x y) = Node  x     (y - 1)
move Right (Node x y) = Node (x + 1) y

class Virus a where
  clean :: a
  infected :: a
  turn :: a -> Direction -> Direction
  modify :: a -> a

-- Instead of just returning the next state, this function also returns the new
-- node state. This is is mostly a performance consideration. While it's
-- possible to compare two infection states to figure out if an infection was
-- added, that's slower than simply returning this data as it's being decided.
-- Not the prettiest design, but it gets the job done...
burst :: Virus a => InfectionState a -> (a, InfectionState a)
burst state =
  let currentNodeState =
        Map.findWithDefault clean (carrierPosition state) (nodes state)

      newDirection = turn currentNodeState $ carrierDirection state

      newNodeState = modify currentNodeState
      newNodes = Map.insert (carrierPosition state) newNodeState (nodes state)

      newPosition = move newDirection $ carrierPosition state
  in (newNodeState, InfectionState newPosition newDirection newNodes)

countInfections :: (Virus a, Eq a) => Int -> InfectionState a -> Int
countInfections iterations =
  length . filter (== infected) . take iterations . unfoldr (Just . burst)

-- Parsing

index2d :: [[t]] -> [(Node, t)]
index2d rows = [(Node x y, a) | (row, y) <- zip rows (fmap negate [0..]),
                                (  a, x) <- zip row  [0..]]

middle :: [(Node, t)] -> Node
middle ns =
  let (Node  upperLeftX  upperLeftY, _) = head ns
      (Node lowerRightX lowerRightY, _) = last ns
      width  = lowerRightX - upperLeftX
      height = lowerRightY - upperLeftY
  in Node (width `div` 2) (height `div` 2)

parseInput :: (Char -> a) -> String -> InfectionState a
parseInput parseChar inp =
  let indexedNodes = index2d $ lines inp
      pos = middle indexedNodes
      ns = Map.fromList $ fmap (fmap parseChar) indexedNodes
  in InfectionState pos Up ns

-- Puzzle 1

data NodeState1 = Clean1 | Infected1 deriving (Show, Eq)

parseNodeState1 :: Char -> NodeState1
parseNodeState1 '#' = Infected1
parseNodeState1  _  = Clean1

instance Virus NodeState1 where
  clean = Clean1

  infected = Infected1

  turn    Clean1 =  turnLeft
  turn Infected1 = turnRight

  modify    Clean1 = Infected1
  modify Infected1 =    Clean1

solve1 :: String -> Int
solve1 = countInfections 10e3 . parseInput parseNodeState1

-- Puzzle 2

data NodeState2 = Clean2 | Weakened2 | Infected2 | Flagged2 deriving (Show, Eq)

parseNodeState2 :: Char -> NodeState2
parseNodeState2 '#' = Infected2
parseNodeState2 'W' = Weakened2
parseNodeState2 'F' = Flagged2
parseNodeState2  _  = Clean2

instance Virus NodeState2 where
  clean = Clean2

  infected = Infected2

  turn    Clean2 = turnLeft
  turn Weakened2 = id
  turn Infected2 = turnRight
  turn  Flagged2 = reverseDirection

  modify    Clean2 = Weakened2
  modify Weakened2 = Infected2
  modify Infected2 = Flagged2
  modify  Flagged2 = Clean2

-- Takes some six minutes to run on my machine...
solve2 :: String -> Int
solve2 = countInfections 10e6 . parseInput parseNodeState2

-- Tests

example1 :: String
example1 = "..#\n\
           \#..\n\
           \..."

test :: Bool
test = and
  [
       5 == countInfections   7 (parseInput parseNodeState1 example1),
      41 == countInfections  70 (parseInput parseNodeState1 example1),
    5587 == solve1 example1,
      26 == countInfections 100 (parseInput parseNodeState2 example1)
  ]
