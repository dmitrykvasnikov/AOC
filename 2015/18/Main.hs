module Main where

import Data.Set qualified as S
import Data.List ((\\), foldl')
import Control.Monad (guard)

type Coord = (Int, Int)
type Size = Int
type Grid = S.Set Coord
type Input = S.Set (Int, Int)

corners :: Input
corners = S.fromList [(1,1), (1,100), (100,1), (100,100)]

getInput :: FilePath -> IO Input
getInput filepath = do
  input <- getGrid . lines <$> readFile filepath
  return input

getGrid :: [String] -> Input
getGrid str = S.fromList $ do
  (y, row) <- zip [1..] str
  (x, p)   <- zip [1..] row
  guard (p == '#')
  return (x,y)

getNbors :: Size -> Coord -> Input
getNbors m (x,y) = S.fromList . ( \\ [(x,y)]) $ do
  dx <- [-1..1]
  dy <- [-1..1]
  let x' = x + dx
  let y' = y + dy
  guard ((x' > 0) && (x' <= m)  && (y' > 0) && (y' <= m))
  return (x + dx, y + dy)

getActiveNbors :: Input -> Size -> Coord -> Input
getActiveNbors grid m = S.filter (flip S.member grid) . getNbors m

getNonActiveNbors :: Input -> Size -> Coord -> Input
getNonActiveNbors grid m = S.filter (not . flip S.member grid) . getNbors m

getOffGrid :: Input -> Size -> Input
getOffGrid grid m = S.filter check grid
  where check cell =
          let activeNbors = S.size $ getActiveNbors grid m cell
          in if activeNbors < 2 || activeNbors > 3 then True else False

getOnGrid :: Input -> Size -> Input
getOnGrid grid m = foldl' step S.empty (S.toList grid)
  where step off cell = S.union off (S.filter check $ getNonActiveNbors grid m cell)
        check candidate = case S.size $ getActiveNbors grid m candidate of
                            3 -> True
                            otherwise -> False

step :: Size -> Input -> Input
step m grid = let (on,off) = ((,) <$> flip getOnGrid m <*> flip getOffGrid m) grid
            in S.union on $ S.difference grid off
step2 :: Size -> Input -> Input
step2 m grid = let (on,off) = ((,) <$> flip getOnGrid m <*> flip getOffGrid m) grid
            in S.union corners $ S.union on $ S.difference grid off

part1 :: Input -> Int
part1 = S.size . head . drop 100 . iterate (step 100)

part2 :: Input -> Int
part2 grid = S.size . head . drop 100 $ iterate (step2 100) (S.union corners grid)

main :: IO()
main = do
  input <- getInput "./input.txt"
  putStrLn $ "Part 1: " <> show (part1 input)
  putStrLn $ "Part 2: " <> show (part2 input)
