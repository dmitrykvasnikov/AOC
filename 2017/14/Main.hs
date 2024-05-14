module Main where

import AOC
import Data.Maybe (fromMaybe, catMaybes)
import Text.Regex.Applicative (RE, match, (=~), sym, psym, string, many, some, (<|>), reFoldl)
import Text.Regex.Applicative.Common (decimal)
import Data.Char (ord)
import Data.Set qualified as S
import Data.List (foldl1, foldl', (\\))
import Data.Bits
import GHC.Base (unsafeChr)
import System.Posix.Internals (fileType)
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

type Position = Int
type Offset = Int
type Hash = [Int]

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

hash :: String -> String
hash = concatMap toHex . dense . (\(x,_,_) -> x) .  foldl' step ([0..255], 0, 0) . concat . replicate 64 . (++ [17, 31, 73, 47, 23]) . map ord

getGrid :: Input -> [String]
getGrid = map (concatMap toBin . hash)

type Input = [String]

input :: Input
input = zipWith (<>) (repeat "jxqlasbh-") (map show [0..127])

getUsed :: String -> Int
getUsed = length . filter (== '1')

toBin :: Char -> String
toBin '0' = "0000"
toBin '1' = "0001"
toBin '2' = "0010"
toBin '3' = "0011"
toBin '4' = "0100"
toBin '5' = "0101"
toBin '6' = "0110"
toBin '7' = "0111"
toBin '8' = "1000"
toBin '9' = "1001"
toBin 'a' = "1010"
toBin 'b' = "1011"
toBin 'c' = "1100"
toBin 'd' = "1101"
toBin 'e' = "1110"
toBin 'f' = "1111"

isWall :: Input -> (Int,Int) -> Bool
isWall g (x,y) = ((g !! y) !! x) == '1'

getNbrs :: (Int, Int) -> [(Int, Int)]
getNbrs (x,y) = [(x', y') | (x', y') <- [(x-1, y), (x+1,y), (x, y-1), (x,y+1)], x' >= 0, y' >= 0, x' < 128, y' < 128]

getIslands :: Input -> [(Int,Int)] -> Int -> Int
getIslands g [] r = r
getIslands g (c:cs) r
  | not $ isWall g c = getIslands g cs r
  | otherwise        = getIslands g (cs \\ bfs [c] [c]) (r + 1)
     where bfs [] s = s
           bfs (q:qs) s = bfs (qs ++ walls) (s ++ nbrs)
             where nbrs = getNbrs q
                   walls = filter (\n -> (not $ elem n s) && isWall g n) nbrs

part1 :: Input -> Int
part1 = sum . map getUsed . getGrid

part2 :: Input -> Int
part2 i = getIslands (getGrid i) [(x,y) | x <- [0..127], y <- [0..127]] 0

main :: IO()
main = do
  putStrLn $ "Part 1: " <> show (part1 input)
  putStrLn $ "Part 2: " <> show (part2 input)
