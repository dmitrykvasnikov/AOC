module Main where

import Data.Char (isAlpha, ord, chr)
import Data.List (concat, group, groupBy,  sort, sortOn, isPrefixOf)
import Data.Maybe (fromMaybe, catMaybes)
import Text.Regex.Applicative (RE, match, (=~), sym, psym, string, many, some, (<|>))
import Text.Regex.Applicative.Common (decimal)

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

type Key = String
type Hash = String
type ID = Int
data Rooms = Rooms { keys :: [Key], id' :: ID, hash :: Hash } deriving Show

rooms :: Parser Rooms
rooms = Rooms <$> some (many (psym isAlpha) <* sym '-') <*> decimal <* sym '[' <*> many (psym isAlpha) <* sym ']'

name :: [Key] -> String
name = concat . map sort . (map $ map (\a -> fst a)) . groupBy (\a b -> snd a == snd b) . map (\cs -> (head cs, length cs)) . reverse . sortOn length . group . sort . concat

shift :: Int -> Char -> Char
shift m c = chr ((((ord c - 97) + m) `mod` 26) + 97)

type Input = [Rooms]

getInput :: FilePath -> IO Input
getInput filepath = do
  input <- catMaybes . map (match rooms) . lines <$> readFile filepath
  return input

validRooms :: [Rooms] -> [Rooms]
validRooms = filter (\(Rooms k _ h) -> isPrefixOf h (name k))

part1 :: Input -> Int
part1 = sum . map id' . validRooms
part2 :: Input -> Int
part2 = snd . head . filter (\(n, _) -> isPrefixOf "north" n) .  map (\(Rooms k i _) -> (map (shift i) (concat k), i)) . validRooms

main :: IO()
main = do
  input <- getInput "./input.txt"
  putStrLn $ "Part 1: " <> show (part1 input)
  putStrLn $ "Part 2: " <> show (part2 input)
