module Main where

import Data.Set as S
import Data.List
import Data.Foldable as F

type Home = (Int, Int)
type Counter = Int
type Direction = Char
type Area = S.Set Home

splitInput :: String -> (String, String)
splitInput input = go input ("", "")
  where
    go [] res = res
    go (x:[]) (res1, res2) = (res1 ++ [x], res2)
    go (x:y:rest) (res1, res2) = go rest (res1 ++ [x], res2 ++ [y])


moveAddress :: Home -> Direction -> Home
moveAddress (x,y) dir
  | dir == '^' = (x, y - 1)
  | dir == 'v' = (x, y + 1)
  | dir == '>' = (x + 1, y)
  | dir == '<' = (x - 1, y)
  | otherwise = error $ "Wrong directrion provided: " ++ [dir]

startHome :: Area
startHome = S.singleton (0,0)

updateArea :: (Area, Home, Counter) -> Direction -> (Area, Home, Counter)
updateArea (area, home, cnt) dir =
  let
    newHome = moveAddress home dir
  in case S.member newHome area of
    True -> (area, newHome, cnt)
    False -> (S.insert newHome area, newHome, cnt + 1)

foldArea input start = F.foldl' updateArea (start, (0,0), 1) input

part1 :: FilePath -> IO Int
part1 filepath = do
  input <- head . lines <$> readFile filepath
  let (_, _, result) = foldArea input startHome
  return result

part2 :: FilePath -> IO Int
part2 filepath = do
  (input1, input2) <- splitInput . head . lines <$> readFile filepath
  let (area, _, result1) = foldArea input1 startHome
  let (_, _, result2) = foldArea input2 area
  return $ result1 + result2 - 1

main :: IO()
main = do
  part1 "./input.txt" >>= \res -> putStrLn $ "Part 1 :" <> show res
  part2 "./input.txt" >>= \res -> putStrLn $ "Part 2 :" <> show res
