module Main where

import Data.Bits
import Control.Monad
import Data.Maybe (fromMaybe, catMaybes, fromJust)
import Text.Regex.Applicative (RE, match, (=~), sym, psym, string, many, some, (<|>))
import Text.Regex.Applicative.Common (decimal)
import Data.List (foldl')
import Data.Set (Set)
import Data.Set qualified as S
import Data.Map.Strict (Map)
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
type Walls = Set Coord
type Dp = Map Coord Int
type Queue = [Coord]

type Input = String

walls :: Int -> Int -> Int -> Walls
walls w h magic =
  foldl' go S.empty [(x,y) | x <- [0..(w-1)], y <- [0..(h-1)]]
    where go set (x,y) =
            case odd . popCount $  (x*x + 3*x + 2*x*y + y + y*y + magic) of
              True -> S.insert (x,y) set
              False -> set

-- /** neighbours
neighbours :: Coord -> Int -> Int -> [Coord]
neighbours (x,y) w h = do
  dx <- [-1..1]
  dy <- [-1..1]
  guard  (abs dx /= abs dy)
  let x' = x + dx
  let y' = y + dy
  guard (x'>=0 && x' <= (w-1) && y' >= 0 && y' <= (h-1))
  return (x', y')
-- **/
nbors' q w h wls = filter (not . flip S.member wls) $ neighbours q w h

dp :: Int -> Int -> Walls -> Dp -> Queue -> Dp
dp w h wls = go
  where go :: Dp -> Queue -> Dp
        go dp [] = dp
        go dp (q:qs) = go (M.insert q dst dp) (qs ++ qs')
          where nbors = filter (not . flip S.member wls) $ neighbours q w h
                dst = succ . minimum . catMaybes . map (flip M.lookup dp) $ nbors
                qs' = filter (not . flip M.member dp) nbors

getInput :: FilePath -> IO Input
getInput filepath = do
  input <- prepare <$> readFile filepath
  return input

prepare :: String -> Input
prepare = undefined

part1 :: Coord -> Int -> Int -> Int -> Int
part1 coord w h magic = let wls = walls w h magic
                            emptyDp = M.fromList [((1,1), 0)]
                            q = filter (not . flip S.member wls) $ neighbours (1,1) w h
                        in fromJust . M.lookup coord $ (dp w h wls emptyDp q)

part2 w h magic = let wls = walls w h magic
                      emptyDp = M.fromList [((1,1), 0)]
                      q = filter (not . flip S.member wls) $ neighbours (1,1) w h
                  in M.size . M.filter (<= 50) $ (dp w h wls emptyDp q)

main :: IO()
main = do
  input <- getInput "./input.txt"
  putStrLn $ "Part 1: " <> show (part1 (31,39) 50 50 1362)
  putStrLn $ "Part 2: " <> show (part2 50 50 1362)
