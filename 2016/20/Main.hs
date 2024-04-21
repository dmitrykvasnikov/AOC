module Main where

import Data.Maybe (fromMaybe, catMaybes)
import Text.Regex.Applicative (RE, match, (=~), sym, psym, string, many, some, (<|>))
import Text.Regex.Applicative.Common (decimal)
import Data.List (sortOn)
import Data.List.Split (splitOn)
import GHC.OldList (zipWith4)

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

type Range = (Int,Int)
type Input = [Range]

isOverlap :: Range -> Range -> Bool
isOverlap (_, e1) (s2, _) = e1 >= (s2 - 1)

zipRanges :: Range -> Range -> Range
zipRanges (s1, e1) (_, e2) = (s1, max e1 e2)

zipAllRanges :: [Range] -> [Range]
zipAllRanges [] = []
zipAllRanges [x] = [x]
zipAllRanges (x:y:rest) =
  case isOverlap x y of
    True -> zipAllRanges $ (zipRanges x y) : rest
    False -> x : zipAllRanges (y:rest)

getInput :: FilePath -> IO Input
getInput filepath = do
  input <- prepare <$> readFile filepath
  return input

prepare :: String -> Input
prepare = sortOn fst . map (makeRange . splitOn "-") . lines
  where makeRange :: [String] -> Range
        makeRange [x,y] = (read x, read y)
        makeRange _        = error "Parse error"

part1 :: Input -> Int
part1 = succ . snd . head . zipAllRanges

part2 :: Input -> Int
part2 = getIPs . zipAllRanges . flip (++) [(4294967296, 4294967296)]
  where getIPs [x] = 0
        getIPs ((_,e1) :y@(s2,_) :rest) = (s2 - e1 - 1) + getIPs (y:rest)

main :: IO()
main = do
  input <- getInput "./input.txt"
  putStrLn $ "Part 1: " <> show (part1 input)
  putStrLn $ "Part 2: " <> show (part2 input)
