module Main where

import Data.Maybe (fromMaybe, catMaybes)
import Text.Regex.Applicative (RE, match, (=~), sym, psym, string, many, some, (<|>))
import Text.Regex.Applicative.Common (decimal)
import Data.List (nub, (\\), foldl')
import Data.Set qualified as S
import Debug.Trace
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

type Coord = (Int, Int)
type Walls = S.Set Coord

dfs, bfs :: Eq a => a -> (a -> Bool) -> (a -> [a]) -> [a]
bfs x isGoal adj
  | isGoal x  = [x]
  | otherwise = genericSearch [[x]] [] isGoal adj (\open newOpen -> open ++ newOpen)
dfs x isGoal adj
  | isGoal x  = [x]
  | otherwise = genericSearch [[x]] [] isGoal adj (\open newOpen -> newOpen ++ open)

genericSearch :: Eq a => [[a]] -> [[a]] -> (a -> Bool) -> (a -> [a]) -> ([[a]] -> [[a]] -> [[a]]) -> [a]
genericSearch open closed isGoal adj modifyOpen
  | length solution > 0 =  head solution
  | otherwise = genericSearch (modifyOpen pathList newOpen) newClosed isGoal adj modifyOpen
  where
    currPath@(currState:_) = head open
    newClosed = currPath:closed
    pathList = tail open
    newOpen = map (\x -> x:currPath) (adj currState) \\ newClosed
    solution = filter (\(st:_) -> isGoal st) $ newOpen

walls :: [String] -> Walls
walls str = S.fromList $ [(x,y) | (y, raw) <- zip [-1..] str,
                     (x, ch) <- zip [-1..] raw, ch == '#']

deltas :: [Coord]
deltas = [(-1,0), (0,1), (0,-1), (1,0)]

adjacents :: Walls -> Coord -> [Coord]
adjacents walls coord = filter (not . flip S.member walls) (map (getAdjacents coord) deltas)
  where getAdjacents (x1,y1) (x2,y2) = (x1+x2, y1+y2)

type Input = Walls

getInput :: FilePath -> IO Input
getInput filepath = do
  input <- prepare <$> readFile filepath
  return input

prepare :: String -> Input
prepare = walls . lines

part1 :: Walls -> [Coord]
part1 w = bfs (0,0) (\x -> x == (6,2)) (adjacents w)

part2 :: Input -> Int
part2 input = 42

main :: IO()
main = do
  input <- getInput "./input.txt"
  putStrLn $ "Part 1: " <> show (part1 input)
  putStrLn $ "Part 2: " <> show (part2 input)

