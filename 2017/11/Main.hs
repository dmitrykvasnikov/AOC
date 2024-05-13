module Main where

import AOC
import Data.Maybe (fromMaybe, catMaybes)
import Text.Regex.Applicative (RE, match, (=~), sym, psym, string, many, some, (<|>))
import Text.Regex.Applicative.Common (decimal)
import Data.List.Split
import Data.List (foldl')

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

type Direction = String
type Input = [String]

move :: (Coord, Int) -> Direction -> (Coord, Int)
move (c, maxD) d = let newC = case d of
                        "n" -> Coord (_x c) (_y c - 1)
                        "s" -> Coord (_x c) (_y c + 1)
                        "e" -> Coord (_x c + 1) (_y c)
                        "w" -> Coord (_x c - 1) (_y c)
                        "nw" -> Coord (_x c - 0.5) (_y c - 0.5)
                        "ne" -> Coord (_x c + 0.5) (_y c - 0.5)
                        "sw" -> Coord (_x c - 0.5) (_y c + 0.5)
                        "se" -> Coord (_x c + 0.5) (_y c + 0.5)
                    in (newC, max maxD (getDistance newC))

getEndpoint :: Input -> (Coord, Int)
getEndpoint = foldl' move ((Coord 0 0), 0)

getDistance :: Coord -> Int
getDistance c = round (abs (_x c) + abs (_y c))

prepare :: String -> Input
prepare = splitOn "," . head . lines

part1 :: Input -> Int
part1 = getDistance . fst . getEndpoint

part2 :: Input -> Int
part2 = snd . getEndpoint

main :: IO()
main = do
  input <- prepare <$> readFile "./input.txt"
  putStrLn $ "Part 1: " <> show (part1 input)
  putStrLn $ "Part 2: " <> show (part2 input)
