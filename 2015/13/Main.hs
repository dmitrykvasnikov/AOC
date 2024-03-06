module Main where

import Data.Char (isAlpha)
import Data.Map.Strict qualified as M
import Text.Regex.Applicative (RE, match, string, sym, psym, some, (<|>))
import Text.Regex.Applicative.Common (decimal)

type Name = String
type Neighbour = String
type Input = String
type Happiness = Int
type Value = M.Map Name (M.Map Neighbour Happiness)
type Allocation = (Name, Happiness, Neighbour)

type Parser a = RE Char a

word :: Parser String
word = some (psym isAlpha)

happy :: Parser Int
happy = (string " would gain " *> decimal) <|> (string " would lose " *> ((negate) <$> decimal))

allocation :: Parser Allocation
allocation = (,,) <$> word <*> happy <* string " happiness units by sitting next to " <*> word <* sym '.'

getInput :: FilePath -> IO Value
getInput filepath = undefined

part1 :: Value -> Int
part1 input = 42

part2 :: Value -> Int
part2 input = 42

main :: IO()
main = do
  input <- getInput "./input.txt"
  putStrLn $ "Part 1: " <> show (part1 input)
  putStrLn $ "Part 2: " <> show (part1 input)
