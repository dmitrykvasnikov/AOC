module Main where

import Data.Maybe (fromJust, catMaybes)
import Text.Regex.Applicative (match, RE, (<|>), string, sym, many)
import Text.Regex.Applicative.Common (decimal)
import Data.Set qualified as S

data Coord = Coord { _y, _x :: Int } deriving (Show, Eq, Ord)
data Turn = L | R deriving (Show, Eq)
data Direction = N | E | S | W deriving (Show, Eq, Ord, Enum, Bounded)
data Move = Move Turn Int deriving Show
type Position = (Direction, Coord)

type Input = [Move]

type Parser a = RE Char a

turnP :: Parser Turn
turnP = (L <$ sym 'L') <|> (R <$ sym 'R')

moveP :: Parser Move
moveP = Move <$> turnP <*> decimal

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy parser sep = (:) <$> parser <*> many (sep *> parser)

succ' :: Direction -> Direction
succ' dir = toEnum ((fromEnum dir + 1 ) `mod` (1+ fromEnum (maxBound :: Direction)))

pred' :: Direction -> Direction
pred' dir = toEnum ((fromEnum dir - 1 ) `mod` (1 + fromEnum (maxBound :: Direction)))

turn :: Turn -> Direction -> Direction
turn L = pred'
turn R = succ'

move :: Position -> Move -> Position
move m@(dir, c@(Coord y x)) (Move t l) =
  let dir' = turn t dir
  in case dir' of
      N -> (dir', c { _y = y - l})
      E -> (dir', c { _x = x + l})
      S -> (dir', c { _y = y + l})
      W -> (dir', c { _x = x - l})

getCoords :: Position -> Position -> [Coord]
getCoords (_, Coord y1 x1) (_, Coord y2 x2)
  | x1 == x2 = let d = if y2 > y1 then 1 else (-1)
               in tail [Coord y' x1 | y' <- enumFromThenTo y1 (y1 + d) y2]
  | y1 == y2 = let d = if x2 > x1 then 1 else (-1)
               in tail [Coord y1 x' | x' <- enumFromThenTo x1 (x1 + d) x2]

checkCoords :: [Coord] -> S.Set Coord -> Maybe Coord
checkCoords [] _    = Nothing
checkCoords (c:cs) s = if S.member c s then Just c else checkCoords cs s

getInput :: FilePath -> IO Input
getInput filepath = do
  input <- fromJust . match (sepBy moveP (string ", ")) . head . lines <$>  readFile filepath
  return input

part1 :: Input -> Int
part1 = go . foldl move (N, Coord 0 0)
  where go (_, Coord x y) = abs x + abs y

part2 :: Input -> Int
part2 = go (N, Coord 0 0) $ S.singleton (Coord 0 0)
  where go _ _ []        = -1
        go pos1 s (m:ms) =
          let pos2 = move pos1 m
              coords = getCoords pos1 pos2
              coord = checkCoords coords s
          in case coord of
              Just (Coord y x) -> abs y + abs x
              Nothing -> go pos2 (S.union s (S.fromList coords)) ms

main :: IO()
main = do
  input <- getInput "./input.txt"
  putStrLn $ "Part 1: " <> show (part1 input)
  putStrLn $ "Part 2: " <> show (part2 input)
