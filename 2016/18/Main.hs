module Main where

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

traps :: [String]
traps = ["^^.", ".^^", "^..", "..^"]

genTile :: String -> Char
genTile s = case elem s traps of
          True -> '^'
          False -> '.'

genRaw :: String -> String
genRaw s@(x:y:z:rest) = genTile (take 3 s) : genRaw (tail s)
genRaw _            = []

type Input = String

getInput :: FilePath -> IO Input
getInput filepath = do
  input <- prepare <$> readFile filepath
  return input

prepare :: String -> Input
prepare = undefined

part1 :: Int -> Input -> Int
part1 n  = sum . take n . map (length . filter (== '.')) . iterate go
            where go str = genRaw $ "." ++ str ++ "."

part2 :: Input -> Int
part2 input = 42

main :: IO()
main = do
  let input = "^..^^.^^^..^^.^...^^^^^....^.^..^^^.^.^.^^...^.^.^.^.^^.....^.^^.^.^.^.^.^.^^..^^^^^...^.....^....^."
  putStrLn $ "Part 1: " <> show (part1 40 input)
  putStrLn $ "Part 2: " <> show (part1 400000 input)
