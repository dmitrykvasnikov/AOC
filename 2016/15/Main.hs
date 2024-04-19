module Main where

import Data.Maybe (fromMaybe, catMaybes)
import Text.Regex.Applicative (RE, match, (=~), sym, psym, string, many, some, (<|>))
import Text.Regex.Applicative.Common (decimal)
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

data Clock = Clock {len, st:: Int} deriving Show
type Time = Int
type Input = [Clock]

clock :: Parser Clock
clock = Clock <$ string "Disc #" <* decimal <* string " has " <*> decimal <* string " positions; at time=0, it is at position " <*> decimal <* sym '.'

isClockAtZero :: Clock -> Time -> Bool
isClockAtZero clock time = ((st clock + time) `mod` (len clock)) == 0

getInput :: FilePath -> IO Input
getInput filepath = do
  input <- prepare <$> readFile filepath
  return input

prepare :: String -> Input
prepare = catMaybes . map (match clock) . lines

part1 :: Input -> Int
part1 input = head . filter isValid $ (enumFromThen time (step + 1))
               where step = len . head $ input
                     time = step - (st . head $ input) - 1
                     isValid t = all (== True) (zipWith isClockAtZero input [(t + 1)..])

part2 :: Input -> Int
part2 input = part1 $ input ++ [Clock 11 0]

main :: IO()
main = do
  input <- getInput "./input.txt"
  putStrLn $ "Part 1: " <> show (part1 input)
  putStrLn $ "Part 2: " <> show (part2 input)
