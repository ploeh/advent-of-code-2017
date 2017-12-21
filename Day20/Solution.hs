module Day20 where

import Data.Function (on)
import Data.Ord (comparing)
import Data.List (minimumBy, groupBy)
import Data.Semigroup (Semigroup(..), Endo(..), stimesMonoid)
import Text.ParserCombinators.ReadP

data Vector =
  Vector { x :: Integer, y :: Integer, z :: Integer } deriving (Show, Eq)

instance Semigroup Vector where
  (Vector x0 y0 z0) <> (Vector x1 y1 z1) = Vector (x0 + x1) (y0 + y1) (z0 + z1)

instance Monoid Vector where
  mempty = Vector 0 0 0
  mappend = (<>)

data Particle =
  Particle { position :: Vector, velocity :: Vector, acceleration :: Vector }
  deriving (Show, Eq)

-- Parsing

parseNumber :: ReadP Integer
parseNumber =
  fmap read
  $ pure (++)
  <*  skipSpaces
  <*> option "" (string "-")
  <*> munch (`elem` ['0'..'9'])
  <*  skipSpaces

parseVector :: ReadP Vector
parseVector =
  Vector
  <$> (char '<' *> parseNumber <* char ',')
  <*> (            parseNumber <* char ',')
  <*> (            parseNumber <* char '>')

parseParticle :: ReadP Particle
parseParticle =
  Particle
  <$> (string "p=" *> parseVector <* char ',' <* skipSpaces)
  <*> (string "v=" *> parseVector <* char ',' <* skipSpaces)
  <*> (string "a=" *> parseVector             <* skipSpaces)

parseLine :: ReadS Particle
parseLine = readP_to_S (parseParticle <* eof)

parseInput :: String -> [Particle]
parseInput =
  concatMap (fmap fst . filter ((== "") . snd)) . fmap parseLine . lines

-- Puzzle 1

vectorDistance :: Vector -> Vector -> Integer
vectorDistance (Vector x0 y0 z0) (Vector x1 y1 z1) =
  abs (x1 - x0) + abs (y1 - y0) + abs (z1 - z0)

particleDistance :: Particle -> Particle -> Integer
particleDistance p0 p1 = vectorDistance (position p0) (position p1)

origin :: Particle
origin = Particle mempty mempty mempty

findClosest :: (Enum a, Num a) => [Particle] -> (a, Integer)
findClosest =
  minimumBy (comparing snd) . zip [0..] . fmap (particleDistance origin)

tickParticle :: Particle -> Particle
tickParticle (Particle p v a) =
  let newVelocity =      v <> a
      newPosition = p <> v <> a
  in Particle newPosition newVelocity a

tick :: Functor f => f Particle -> f Particle
tick = fmap tickParticle

-- Instead of deterministically solving this puzzle, this simulation runs an
-- infinite round of ticks. From GHCi, I then ran
-- λ> take 1000 $ simulate1 inp
-- which completed in about 10 seconds, and definitely seemed to have stabilised
-- on a single particle long before the 1000th iteration. It also produced the
-- correct result according to the Advent of Code web site, given my puzzle
-- input.
simulate1 :: String -> [Integer]
simulate1 = fmap (fst . findClosest) . iterate tick . parseInput

-- As a general observation, given that acceleration is constant, particles
-- eventually don't change direction (I think), so all moving particles must
 -- eventually move away from the origin. Thus, if there are any non-moving
-- particles, one of these must eventually be the closest particle.
-- Unfortunately, my puzzle input has no non-moving particles, so the problem is
-- harder than that.
-- When all particles are moving, since they're all bound to eventually move
-- away from the origin, the particle with the lowest acceleration must then be
-- the one that will eventually be closest to the origin. Thus, this solution
-- simply finds the particle with the lowest acceleration.
-- I only started to think about this after I'd run and submitted the above
-- simulation, so I haven't thought this through, but this function, at least,
-- produces the same outcome as my simulation, so it seems that it may hold.
-- It may be sheer luck that there's only a single particle with the minimum
-- acceleration, or that the first one found is the correct answer. In case of a
-- tie, one would have to compare initial velocities instead.
solve1 :: String -> Integer
solve1 =
  fst
  . minimumBy (comparing $ vectorDistance mempty . acceleration . snd)
  . zip [0..]
  . parseInput

-- Puzzle 2

removeCollisions :: [Particle] -> [Particle]
removeCollisions =
  concat . filter ((== 1) . length) . groupBy ((==) `on` position)

-- Produces an infinite iteration of swarms, given an initial particle swarm.
run2 :: [Particle] -> [Int]
run2 = fmap length . iterate (removeCollisions . tick)

-- Like the case of puzzle 1, instead of deterministically solving this puzzle,
-- this simulation runs an infinite round of ticks, including removing collided
-- particules. From GHCi, I then ran
-- λ> take 1000 $ simulate2 inp
-- which completed in about 5 seconds, and definitely seemed to have stabilised
-- on a size long before the 1000th iteration. It also produced the correct
-- result according to the Advent of Code web site, given my puzzle input.
simulate2 :: String -> [Int]
simulate2 = run2 . parseInput

-- Tests

example1 :: String
example1 = "p=< 3,0,0>, v=< 2,0,0>, a=<-1,0,0>\n\
           \p=< 4,0,0>, v=< 0,0,0>, a=<-2,0,0>"

example2 :: String
example2 = "p=<-6,0,0>, v=< 3,0,0>, a=< 0,0,0>\n\
           \p=<-4,0,0>, v=< 2,0,0>, a=< 0,0,0>\n\
           \p=<-2,0,0>, v=< 1,0,0>, a=< 0,0,0>\n\
           \p=< 3,0,0>, v=<-1,0,0>, a=< 0,0,0>"

px0, px1 :: Particle
px0 = head $ parseInput example1
px1 = parseInput example1 !! 1

swarmx :: [Particle]
swarmx = parseInput example2

tickTimesParticle :: Integer -> Particle -> Particle
tickTimesParticle n = appEndo $ stimesMonoid n $ Endo tickParticle

test :: Bool
test = and
  [
    Particle (Vector   4  0 0) (Vector   1  0 0) (Vector (-1) 0 0) == tickTimesParticle 1 px0,
    Particle (Vector   4  0 0) (Vector   0  0 0) (Vector (-1) 0 0) == tickTimesParticle 2 px0,
    Particle (Vector   3  0 0) (Vector (-1) 0 0) (Vector (-1) 0 0) == tickTimesParticle 3 px0,
    Particle (Vector   2  0 0) (Vector (-2) 0 0) (Vector (-2) 0 0) == tickTimesParticle 1 px1,
    Particle (Vector (-2) 0 0) (Vector (-4) 0 0) (Vector (-2) 0 0) == tickTimesParticle 2 px1,
    Particle (Vector (-8) 0 0) (Vector (-6) 0 0) (Vector (-2) 0 0) == tickTimesParticle 3 px1,
    0 == solve1 example1,
    [4,4,1,1] == take 4 (run2 swarmx)
  ]
