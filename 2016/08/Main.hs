module Main where

import Data.Maybe (fromMaybe, catMaybes, fromJust)
import Data.List (foldl')
import Text.Regex.Applicative (RE, match, (=~), sym, psym, string, many, some, (<|>))
import Text.Regex.Applicative.Common (decimal)
import Data.Map.Strict qualified as M
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


type Coord = (Int, Int)
data Action = Rect Int Int | Column Int Int | Row Int Int deriving (Show, Eq, Ord)
type Grid = M.Map Coord Char

w, h :: Int
w = 50
h = 6

emptyGrid :: Grid
emptyGrid = M.fromList [((y,x) ,'.') | y <- [0..(h-1)], x <- [0..(w-1)]]

getRow, getColumn :: Grid -> Int -> Grid
getRow grid row = M.filterWithKey (\(y,x) _ -> y == row) grid
getColumn grid column = M.filterWithKey (\(y,x) _ -> x == column) grid

printRow :: Grid -> Int -> String
printRow grid row = foldr go  "" [0..(w-1)]
                    where go x res = fromJust (M.lookup (row, x) grid) : res

printGrid :: Grid -> [String]
printGrid grid = map (printRow grid) [0..(h-1)]

type Input = [Action]

actionP :: Parser Action
actionP = Rect <$ string "rect " <*> decimal <* sym 'x' <*> decimal
      <|> Column <$ string "rotate column x=" <*> decimal <* string " by " <*> decimal
      <|> Row <$ string "rotate row y=" <*> decimal <* string " by " <*> decimal


action :: Grid -> Action -> Grid
action grid (Rect x y) = foldl' go grid [(y', x') | x' <- [0..(x-1)], y' <- [0..(y-1)]]
                          where go g c = M.adjust (const '*') c g
action grid (Column c d) = foldl' go grid [(y', c) | y' <- [0..(h-1)]]
                          where o = getColumn grid c
                                go g c''@(y'', x'') = M.adjust (const (fromJust $ M.lookup (((y'' - d + h) `mod` h ), x'') o)) c'' g
action grid (Row c d) = foldl' go grid [(c, x') | x' <- [0..(w-1)]]
                          where o = getRow grid c
                                go g c''@(y'', x'') = M.adjust (const (fromJust $ M.lookup ((y'', (x'' - d + w) `mod` w )) o)) c'' g

getInput :: FilePath -> IO Input
getInput filepath = do
  input <- prepare <$> readFile filepath
  return input

prepare :: String -> Input
prepare = catMaybes . map (match actionP) . lines

part1 :: Input -> Int
part1 = M.foldl' (\r c -> if c == '.' then r else (r + 1)) 0 . foldl' action emptyGrid

part2 :: Input -> [String]
part2 = printGrid . foldl' action emptyGrid

main :: IO()
main = do
  input <- getInput "./input.txt"
  putStrLn $ "Part 1: " <> show (part1 input)
  mapM_ putStrLn (part2 input)

