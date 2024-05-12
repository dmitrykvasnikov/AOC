module Main where

import AOC
import Data.List (foldl', foldl1)
import Data.Char (ord)
import Data.Maybe (fromMaybe, catMaybes)
import Text.Regex.Applicative (RE, match, (=~), sym, psym, string, many, some, (<|>))
import Text.Regex.Applicative.Common (decimal)
import Data.Bits

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
type Position = Int
type Offset = Int
type Hash = [Int]
type Input = [Int]

toHex :: Int -> String
toHex v = go (div v 16) ++ go (mod v 16)
  where go :: Int -> String
        go 10 = "a"
        go 11 = "b"
        go 12 = "c"
        go 13 = "d"
        go 14 = "e"
        go 15 = "f"
        go x = show x

prepare :: String -> Input
prepare = undefined

input :: Input
input = [83,0,193,1,254,237,187,40,88,27,2,255,149,29,42,100]

input2 :: [Int]
input2 = map ord "83,0,193,1,254,237,187,40,88,27,2,255,149,29,42,100" ++ [17,31,73,47,23]

step :: (Hash, Position, Offset) -> Int -> (Hash, Position, Offset)
step (h, p, o) v
  | p + v <= length h = (take p h ++ reverse (take v (drop p h)) ++ drop (p + v) h, mod (p + v + o) (length h), o + 1)
  | otherwise = let h' = p + v - length h
                    t' = (length h) - p
                    r' = reverse (drop p h ++ take h' h)
                in (drop t' r' ++ drop h' (take p h) ++ take t' r', mod (p + v + o) (length h), o + 1)

dense :: [Int] -> [Int]
dense [] = []
dense xs = (foldl1 xor (take 16 xs)) : (dense $ drop 16 xs)

part1 :: Input -> Int
part1 = product . take 2 . (\(x,_,_) -> x) . foldl' step ([0..255], 0, 0)

part2 :: Input -> String
part2 = concatMap toHex . dense . (\(x,_,_) -> x) .  foldl' step ([0..255], 0, 0)

main :: IO()
main = do
  putStrLn $ "Part 1: " <> show (part1 input)
  putStrLn $ "Part 2: " <> show (part2 (concat $ replicate 64 input2))
