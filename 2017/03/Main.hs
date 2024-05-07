module Main where

import AOC
import Data.Maybe (fromMaybe, catMaybes)
import Data.List (unfoldr)
import Data.Map.Strict qualified as M
import Text.Regex.Applicative (RE, match, (=~), sym, psym, string, many, some, (<|>))
import Text.Regex.Applicative.Common (decimal)
import System.Posix.Internals (o_BINARY)
import GHC.IO.Device (IODeviceType(Directory))

-- /** Utilities
type Parser a = RE Char a

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p s = (:) <$> p <*> many (s *> p)

plus, minus :: forall a . (Enum a, Bounded a) => a -> Int -> a
plus v n = toEnum ((fromEnum v + n) `mod`  (fromEnum (maxBound :: a) + 1))
minus v n = plus v (-n)
inc, dec :: forall a . (Enum a, Bounded a) => a -> a
inc v = plus v 1
dec v = plus v (-1)
-- **/

type Input = Int
type StrArg = (Coord, Direction, Int, Int)
type Mem = M.Map Coord Int

data Direction = U | L | D | R deriving (Eq, Ord, Enum, Bounded, Show)

move :: Direction -> Coord -> Coord
move U (x,y) = (x,y+1)
move L (x,y) = (x-1,y)
move D (x,y) = (x,y-1)
move R (x,y) = (x+1,y)

-- moveSeg :: Int -> Direction -> [Coord] -> [Coord]
-- moveSeg l d c@((x,y):_) = (take l $ scanr move s (replicate l d)) ++ c
  -- where s | d /= U = (x,y)
          -- | otherwise = (x+1,y-1)

-- moveLevel :: [Coord] -> Int -> [Coord]
-- moveLevel c l = foldl' (\c' d -> moveSeg l d c') c [U,L,D,R]

stream :: [Coord]
stream = unfoldr go ((0,0), R, 0, 0)
  where
    go :: StrArg -> Maybe (Coord, StrArg)
    go (c, d, l, togo)
      | togo > 0  = Just (move d c, (move d c, d, l, togo - 1))
      | d == R    = Just (move d c, (move d c, inc d, l + 2, l + 1))
      | otherwise = Just (move (inc d) c, (move (inc d) c, inc d, l, l - 1))

deltas :: [Coord]
deltas = [(0,1), (0, (-1)), (1,0), ((-1), 0), (1,1), (1,-1), (-1,1), (-1,-1)]

getNbrs :: Coord -> [Coord]
getNbrs c = map (c <+>) deltas

getMem :: Mem -> Coord -> Int
getMem m c =
  let nbrs = getNbrs c
      mems = catMaybes . map (flip M.lookup m) $ nbrs
  in case null mems of
      True  -> 0
      False -> sum mems

part1 :: Input -> Int
part1 input = manhattan (0,0) . head . drop (input - 2) $ stream

part2 :: Input -> Int
part2 input = go (M.fromList [((0,0), 1)]) stream
  where go :: Mem -> [Coord] -> Int
        go _ []     = error "Empty stream"
        go m (c:cs) = let v = getMem m c
                      in if v > input then v
                                      else go (M.insert c v m) cs

main :: IO()
main = do
  let input = 277678
  putStrLn $ "Part 1: " <> show (part1 input)
  putStrLn $ "Part 2: " <> show (part2 input)
