module Main where

import AOC
import Debug.Trace
import Data.Map.Strict qualified as M
import Data.List (sortOn)
import Data.Maybe (fromJust)
import Data.Set qualified as S

type Bank = M.Map Int Int

type Input = Bank

input :: Input
input = M.fromList $ zip [1..] [0, 5, 10, 0, 11, 14, 13, 4, 11, 8, 8, 7, 1, 4, 12, 11]

inc :: Int -> Int -> Int -> Int -> Int
inc i s l m
  | i == s    = (l `div` m)
  | otherwise = length $ [x | x <- [i, (i + m)..(s + l)], x >= (s+1) && (x <= (s + l+1))]

step :: Bank -> Bank
step b = let s = getMaxInd b
             l = fromJust . M.lookup s $ b
             m = M.size b
         in M.mapWithKey (\i a -> a + inc i s l m) (M.insert s 0 b)

solve :: Bank -> M.Map [(Int,Int)] Int -> Int -> (Int, Int)
solve b mem cnt = let newBank = step b
                      hash = M.toList newBank
                      done = M.member hash mem
                   in case done of
                        True  -> (cnt + 1, cnt - (fromJust . M.lookup hash $ mem))
                        False -> solve newBank (M.insert hash cnt  mem) (cnt + 1)

getMaxInd :: Bank -> Int
getMaxInd b = let val = maximum . M.elems $ b
               in fst . head . sortOn fst . M.toList . M.filter (== val) $ b

part1 :: Input -> (Int, Int)
part1 input = solve input M.empty 0

main :: IO()
main = do
  putStrLn $ "Part 1: " <> show (part1 input)
