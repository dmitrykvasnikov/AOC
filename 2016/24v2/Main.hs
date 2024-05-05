module Main where

import Data.Maybe (fromJust, catMaybes)
import Data.List (permutations)
import Data.Map.Strict qualified as M

type Point = (Int, Int)
type Points = [Point]
type Walls = Points
type Route = Points
type Seen = Points
type Queue = Points
type DistMatrix = M.Map (Point, Point) Int
type Input = [String]

getDist :: Point -> Point -> DistMatrix -> Int
getDist p1 p2 dm = case M.lookup (p1,p2) dm of
                    Just d -> d
                    Nothing -> fromJust . M.lookup (p2,p1) $ dm

getPoints :: String -> [String] -> Points
getPoints pattern strs = [(x,y) | (y, row) <- zip [-1..] strs
                                , (x, ch)  <- zip [-1..] row
                                , elem ch pattern
                         ]

getRouteLength :: DistMatrix -> Route -> Int
getRouteLength dm (x:y:rest) = getDist x y dm + getRouteLength dm (y:rest)
getRouteLength _ _           = 0

walls, points :: [String] -> Points
walls = getPoints "#"
points = getPoints "1234567"
zero :: [String] -> Point
zero = head . getPoints "0"
deltas :: Points
deltas = [(-1,0), (1,0), (0,-1), (0,1)]

getAdjacents :: Walls -> Point -> Points
getAdjacents w p = filter (not . flip elem w) . map (go p) $ deltas
  where go :: Point -> Point -> Point
        go (x1,y1) (x2,y2) = (x1+x2, y1+y2)

getDistMatrix :: Walls -> Points -> DistMatrix
getDistMatrix w ps = go w ps M.empty
  where go _ [] dm      = dm
        go _ [p] dm     = dm
        go w (p:ps') dm = go w ps' (M.union dm $ getDistPoints w p ps' [[p]] [] M.empty)

getDistPoints :: Walls -> Point -> Points -> [Points] -> Seen -> DistMatrix -> DistMatrix
getDistPoints _ _ _ [] _ dm = dm
getDistPoints w p ps (q:qs) seen dm =
  let
    currentPoint = head q
    adjacents = getAdjacents w currentPoint
    newAdjacents = filter (not . flip elem seen) adjacents
    newSeen = seen ++ newAdjacents
    newQueue = qs ++ map (:q) newAdjacents
    newDM
      | elem currentPoint ps = M.insert (p, currentPoint) (length q - 1) dm
      | otherwise            = dm
  in getDistPoints w p ps newQueue newSeen newDM

getInput :: FilePath -> IO Input
getInput filepath = do
  input <- prepare <$> readFile filepath
  return input

prepare :: String -> Input
prepare = lines

part1 :: Input -> Int
part1 input = let
                w = walls input
                ps = points input
                z = zero input
                dm = getDistMatrix w (z:ps)
                routes = map (z:) (permutations ps)
              in minimum . map (getRouteLength dm) $ routes


part2 :: Input -> Int
part2 input = let
                w = walls input
                ps = points input
                z = zero input
                dm = getDistMatrix w (z:ps)
                routes = map (\r -> [z] ++ r ++ [z]) (permutations ps)
              in minimum . map (getRouteLength dm) $ routes

main :: IO()
main = do
  input <- getInput "./input.txt"
  putStrLn $ "Part 1: " <> show (part1 input)
  putStrLn $ "Part 2: " <> show (part2 input)
