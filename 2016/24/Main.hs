module Main where

import Data.Maybe (fromMaybe, catMaybes, fromJust)
import Data.List (permutations, (\\), foldl')
import Data.Set qualified as S
import Data.Map.Strict qualified as M
import Data.Foldable (Foldable(toList))
import Data.IntMap (member)

type Coord = (Int, Int)
type Points = S.Set Coord
type Walls = Points
type Queue = Points
type Seen = Points
type Pattern = String
type Input = [String]
type DP = M.Map Coord Int
type DM = M.Map (Coord, Coord) Int
type Route = [Coord]

(!+!) :: Coord -> Coord -> Int
(!+!) (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

dp0 :: Walls -> Coord -> Coord -> Int
dp0 w b e = go (S.toList $ getAdjacents w b) [b] (M.singleton b 0)
  where go :: [Coord] -> [Coord] -> DP -> Int
        go [] _ dp = fromJust (M.lookup e dp)
        go (q:qs) seen dp = go newQ newS newDp
          where adj = (S.toList) $ getAdjacents w q
                adj' = filter (not . flip elem seen) adj
                newQ = qs <> adj'
                newS = seen <> adj'
                newDp = if null adj then dp else M.insert q ((getMinDist dp (S.fromList adj)) + 1) dp

dp1 :: Walls -> Points -> DM
dp1 w p = foldl' go M.empty (S.toList p)
  where go :: DM -> Coord -> DM
        go dm point = go' dm [[point]] (S.empty)
          where go' :: DM -> [[Coord]] -> Points -> DM
                go' dm [] _ = dm
                go' dm (q:qs) seen = go' newDm newQueue newSeen
                  where curr :: Coord
                        curr = head q
                        adj = S.filter (not . flip S.member seen) $ getAdjacents w curr
                        newDm = if (S.member curr p) && (length q > 1)
                                   then M.insert (curr, point) (length q - 1) dm
                                   else dm
                        newSeen = S.union seen adj
                        newQueue = qs ++ (map (\x -> x : q) (S.toList adj))

getDistMatrix :: Walls -> Points -> DM
getDistMatrix w p =
  let ps = pairs p
  in M.fromList (map go ps)
    where go (c1,c2) = ((c1,c2), dp0 w c1 c2)

pairs :: Points -> [(Coord, Coord)]
pairs = go . toList
  where go :: [Coord] -> [(Coord, Coord)]
        go [] = []
        go [x] = []
        go (x:xs) = map ((,) x) xs ++ go xs

getAdjacents :: Walls -> Coord -> Points
getAdjacents w c = S.fromList . filter (not . flip S.member w) .  map (go c) $ deltas
  where go (x1, y1) (x2,y2) = (x1 + x2, y1 + y2)

getCoords :: Pattern -> [String] -> S.Set Coord
getCoords pat grid = S.fromList $ [(x,y) | (y, raw) <- zip [-1..] grid,
                     (x, ch) <- zip [-1..] raw, elem ch pat]

getMinDist :: DP -> Points -> Int
getMinDist dp = minimum . catMaybes . map (flip M.lookup dp) . S.toList

bestCand :: Coord -> Points -> Coord
bestCand coord ps = snd (S.foldl' go (maxBound :: Int, (0,0)) ps)
  where go acc@(d, c) cand = let d' = cand !+! coord
                             in if d' < d then (d', cand)
                                          else acc

getRouteLength :: DM -> Route -> Int
getRouteLength _ [] = 0
getRouteLength _ [x] = 0
getRouteLength d (x:y:z) = getLength d x y + (getRouteLength d (y:z))

getLength :: DM -> Coord -> Coord -> Int
getLength dm c1 c2 = case (M.lookup (c1,c2) dm) of
                       Just d -> d
                       Nothing -> fromJust (M.lookup (c2,c1) dm)

walls :: Input -> Walls
walls = getCoords "#"

points :: Input -> Points
points = getCoords "123456789"

zero :: Input -> Coord
zero = head . S.toList . getCoords "0"

deltas :: [Coord]
deltas = [(-1,0), (0,1), (0,-1), (1,0)]

getInput :: FilePath -> IO Input
getInput filepath = do
  input <- prepare <$> readFile filepath
  return input

prepare :: String -> Input
prepare = lines

part1 :: Input -> Int
part1 input = let w = walls input
                  p = points input
                  z = zero input
                  dm = getDistMatrix w (S.insert z p)
                  -- dm = dp1 w (S.insert z p)
                  routes = map (z:) (permutations $ S.toList p)
               in minimum . map (getRouteLength dm) $ routes

part2 :: Input -> Int
part2 input = let w = walls input
                  p = points input
                  z = zero input
                  dm = getDistMatrix w (S.insert z p)
                  -- dm = dp1 w (S.insert z p)
                  routes = map (\r -> [z] ++ r ++ [z]) (permutations $ S.toList p)
               in minimum . map (getRouteLength dm) $ routes

main :: IO()
main = do
  input <- getInput "./input.txt"
  putStrLn $ "Part 1: " <> show (part1 input)
  putStrLn $ "Part 2: " <> show (part2 input)

