module Main where

import AOC
import Data.Map.Strict qualified as M
import Data.List ((\\))
import Data.Maybe (fromMaybe, catMaybes, fromJust)
import Text.Regex.Applicative (RE, match, (=~), sym, psym, string, many, some, (<|>), empty, reFoldl)
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

type Programs = M.Map Int [Int]


type Input = Programs

program :: Parser (Int, [Int])
program = (,) <$> decimal <*> ((string " <-> " *> sepBy decimal (string ", ")) <|> empty)

ifConnected :: Programs -> Int -> Int -> Bool
ifConnected p e s = go [s] [s]
  where go :: [Int] -> [Int] -> Bool
        go [] _ = False
        go (q:qs) v
          | q == e = True
          | otherwise = go (qs++ n) (v ++ n)
              where n = filter (not . flip elem v) . fromJust . M.lookup q $ p

countRoutes :: Programs -> Int
countRoutes i = go (M.keys i) 0
  where
    go :: [Int] -> Int -> Int
    go [] r = r
    go (p:ps) r = go (ps \\ connected) (r+1)
      where connected = filter (ifConnected i p) ps

prepare :: String -> Input
prepare = M.fromList . catMaybes . map (match program) . lines

part1 :: Input -> Int
part1 i = length . filter (ifConnected i 0) . M.keys $ i

part2 :: Input -> Int
part2 = countRoutes

main :: IO()
main = do
  input <- prepare <$> readFile "./input.txt"
  putStrLn $ "Part 1: " <> show (part1 input)
  putStrLn $ "Part 2: " <> show (part2 input)
