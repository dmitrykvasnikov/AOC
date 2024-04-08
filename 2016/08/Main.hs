module Main where

import Data.Maybe (fromMaybe, catMaybes)
import Text.Regex.Applicative (RE, match, (=~), sym, psym, string, many, some, (<|>))
import Text.Regex.Applicative.Common (decimal)
import Data.Map.Strict qualified as M


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
data Action = Rect Int Int | Column Int Int | Row Int Int deriving (Show, Eq, Ord)
type Grid = M.Map Coord Char

emptyGrid = M.fromList [((y,x) ,'.') | y <- [1..5], x <- [1..50]]

type Input = [Action]

action :: Parser Action
action = Rect <$ string "rect " <*> decimal <* sym 'x' <*> decimal
     <|> Column <$ string "rotate column x=" <*> decimal <* string " by " <*> decimal
     <|> Row <$ string "rotate row y=" <*> decimal <* string " by " <*> decimal

getInput :: FilePath -> IO Input
getInput filepath = do
  input <- prepare <$> readFile filepath
  return input

prepare :: String -> Input
prepare = catMaybes . map (match action) . lines

part1 :: Input -> Int
part1 input = 42

part2 :: Input -> Int
part2 input = 42

main :: IO()
main = do
  input <- getInput "./input.txt"
  putStrLn $ "Part 1: " <> show (part1 input)
  putStrLn $ "Part 2: " <> show (part2 input)
