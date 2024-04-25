module Main where

import Data.Graph.AStar
import Data.List (nub, intersperse, transpose, sort)
import Control.Monad
import qualified Data.HashSet as S
import Data.Function
import System.Environment

main :: IO ()
main = do arg <- head <$> getArgs
          let start = read arg
          printMatrix (0, start)
          let (Just solution) = search (0, start)
          mapM_ printSingle $ zip [1..] solution

type GeneratorFloor = Int
type ChipFloor = Int
type HumanFloor = Int
type Pair = (GeneratorFloor, ChipFloor)
type Node = (HumanFloor, [Pair])
type Delta = (Int, Int)

-- start :: [Pair]
-- start = [(0,0),(0,1),(0,1),(2,2),(2,2),(0,0),(0,0),(0,0),(0,0)]


neighbors (human, machines) = S.fromList nodes'
  where
    chipdeltas :: [(Int, (GeneratorFloor, ChipFloor))]
    chipdeltas  = [(idx, (1, 0)) | (idx, (generator, _)) <- zip [0..] machines, generator == human] ++
                  [(idx, (0, 1))      | (idx, (_, chip)) <- zip [0..] machines, chip == human]
    combinations :: [[(Int, (GeneratorFloor, ChipFloor))]]
    combinations = do a@(idx1, (g1, c1)) <- chipdeltas
                      b@(idx2, (g2, c2)) <- chipdeltas
                      if idx1 /= idx2
                      then return [a, b]
                      else return [(idx1, (merge (g1, g2), merge (c1, c2)))]
    merge (a, b) = if a > 0 || b > 0 then 1 else 0

    nodes' :: [Node]
    nodes' = do direction <- [1, -1]
                combination <- combinations
                let machines' = sort $ do
                    (idx, (gb, cb)) <- zip [0..] machines
                    case lookup idx combination of
                      (Just (gd, cd)) -> return (gd * direction + gb, cd * direction + cb)
                      _               -> return (gb, cb)
                guard $ valid machines'
                return (human + direction, machines')
    valid f = all (pred f) f
    pred rest (gen, chip) = inRange gen && inRange chip && (chip == gen || all ((/=chip) . fst) rest)
    inRange = (`elem` [0..3])

dist = const . const $ 1
guess :: Node -> Int
guess (h, m) = delta h + foldr ((+) . uncurry ((+) `on` delta)) 0 m
  where delta a = 3 - a

goal (h, m) = all (== (3, 3)) m

search = aStar neighbors dist guess goal


printSingle (i, xs) = do print i
                         printMatrix xs


printMatrix (h, m) = mapM_ putStrLn . (++["\n"]) . reverse . map concat . transpose $ combined
  where combined = l1 : single "h " h : l2
        l1 = (\d -> "F"++show d ++ " ") <$> [1..4]
        l2 = concatMap double $ zip [0..] m
        single d l = replicate l ".  " ++ [d++" "] ++ replicate (3 - l) ".  "
        double (i, (x, y)) = single ("g"++show i) x : [single ("c"++show i) y]
