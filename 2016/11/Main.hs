{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Main where

import Data.Maybe (fromMaybe, catMaybes)
import Text.Regex.Applicative (RE, match, (=~), sym, psym, string, many, some, (<|>))
import Text.Regex.Applicative.Common (decimal)
import Data.Set qualified as S
import Data.Map.Strict qualified as M
import Debug.Trace
import Data.Hashable
import Data.Hashable.Generic
import Data.HashSet qualified as H
import Data.List (foldl', sort, sortOn, intercalate)
import GHC.Generics
import Data.Graph.AStar

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

data Equipment = G | M deriving (Eq, Ord, Show, Generic, Hashable)
data Rock = T | Pr | S | Pl | R | E | D deriving (Eq, Ord, Show, Generic, Hashable)
type Item = (Rock, Equipment)
type Floor = S.Set Item
type Hash = String
type Elevator = M.Map Int Floor
type States = M.Map Hash Int
type Queue = S.Set State
data State = State {el :: Elevator, fl, len, mag :: Int} deriving (Eq, Ord, Show, Generic, Hashable)

initState :: State
-- initState = State { el = M.fromList [ (1, S.fromList [(S,M), (T,M)])
                               -- , (2, S.fromList [(S,G)])
                               -- , (3, S.fromList [(T,G)])
                               -- (4, S.fromList [])], fl = 1, len = 0 , mag = 0}
-- initState = State { el = M.fromList [ (1, S.fromList [(T,G), (T,M), (Pl,G),(S,G)])
initState = State { el = M.fromList [ (1, S.fromList [(E,G),(E,M),(D,G),(D,M),(T,G), (T,M), (Pl,G),(S,G)])
                              , (2, S.fromList [(Pl,M), (S,M)])
                              , (3, S.fromList [(Pr,G), (Pr,M), (R,G), (R,M)])
                              , (4, S.fromList [])], fl = 1, len = 0, mag = 0 }

showF :: Floor -> String
showF = concat . pairs . Prelude.map show  . S.toList
  where pairs :: [String] -> [String]
        pairs [] = []
        pairs [x] = [x]
        pairs (s1@(_:x1:_):s2@(_:x2:_):rest)
         | x1 == x2 = "(P)":pairs rest
         | otherwise = s1 : pairs (s2:rest)

showE :: State -> String
showE st = show (fl st) <> (intercalate "|" .  Prelude.map ( showF . snd) . M.toList . el $ st)

hash1 :: Floor -> String
hash1 f = let (a,b,c) = go (0,0,0) (S.toList f)
              go r [] = r
              go r [x] = upd r x
              go r@(g,m,p) (x@(r1,_):y@(r2,_):rest)
               | r1 == r2 = go (g,m,p+1) rest
               | otherwise = go (upd r x) (y:rest)
              upd (g,m,p) (_,G) = (g+1,m,p)
              upd (g,m,p) (_,M)   = (g,m+1,p)
          in (show a <> show b <> show c)

hashA :: Elevator -> String
hashA = concat . Prelude.map (hash1 . snd) . M.toList

isValidHash :: Hash -> Bool
isValidHash [] = True
isValidHash (x:y:z:rest) = ((y == '0') || (x == '0' && z == '0')) && isValidHash rest

isGoal :: Hash -> Bool
isGoal [x] = True
isGoal (x:xs) = x == '0' && isGoal xs

getStates :: State -> States ->  [State]
getStates state h = Prelude.map (\s -> s { mag = magic s }) .
                    Prelude.filter ((&&) <$> (isValidHash . hashA . el) <*> (not . flip M.member h . showE) )
                    . concatMap go $ dirs
  where dirs = getDirections state
        moves = getMoves $ (M.!) (el state) (fl state)
        go :: Int -> [State]
        go dir = Prelude.map (\m ->
                      State
                      { fl = dir
                      , len = len state + 1,
                      el = M.adjust (flip S.difference m) (fl state) . M.adjust (S.union m) dir $ (el state) }
                      ) moves

getDirections :: State -> [Int]
getDirections state
  | f == 1    = [2]
  | f == 4    = [3]
  | otherwise = if all (\i -> (S.size $ (M.!) (el state) i) == 0) [1..(f - 1)] then [f+1] else [f-1, f+1]
  where f = fl state

getMoves :: Floor -> [Floor]
getMoves = Prelude.map S.fromList . Prelude.filter isValidPair . go . S.toList
  where go [] = []
        go (x:xs) = [[x]] ++ Prelude.map (:x:[]) xs ++ go xs
        isValidPair [x] = True
        isValidPair ((_,G):(_,G):_) = True
        isValidPair ((_,M):(_,M):_) = True
        isValidPair ((r1, _):(r2,_):_) = r1 == r2

-- elevator :: [State] -> States -> Int -> Int
-- elevator [] _ _ = error "Can not find solution!"
-- elevator (s:ss) states l
  -- | M.member h' states = elevator ss states l
  -- | goal l s = elevator ss (M.insert h' (len s) states) l
  -- | goal l s = len s
  -- | otherwise =  elevator (ss ++ (getStates s)) (M.insert h' (len s) states) l
  -- where h' = showE s

elevator :: Queue -> States -> Int -> Int
elevator q s l
  | S.null q = error "No solution found"
  | goal l state = len state
  | M.member h s = elevator states s l
  | otherwise = elevator (S.union states (S.fromList $ getStates state s)) (M.insert h (len state) s) l
  where (state, states) = choose q
        h = showE state

goal :: Int -> State -> Bool
goal l s  = l == (S.size . flip (M.!) 4 . el $ s)

magic :: State -> Int
magic s = Data.List.foldl' go 0 [1..3]
   where go r i = r + (m i) * (S.size $ (M.!) (el s) i)
         m 1 = 4
         m 2 = 2
         m 3 = 1

choose :: Queue -> (State, Queue)
choose states =
  let pick = S.foldl' (\r s -> if (len s + mag s) <= (len r + mag r) then s else r) (head . S.toList . S.take 1 $ states) (S.drop 1 states)
  in (pick, S.difference states $ S.singleton pick)

eLength :: State -> Int
eLength = sum . Prelude.map (S.size . snd) . M.toList . el

part1 :: State -> Int
-- part1 input = head . reverse . Prelude.map snd . M.toList . elevator [input] M.empty $ eLength input
part1 input = elevator (S.singleton input)  M.empty $ eLength input

dstA :: State -> State -> Int
dstA _ _ = 1

goalA :: State -> Bool
goalA state = goal (eLength state) state

nbrs :: State -> H.HashSet State
nbrs state  = Data.List.foldl' (flip H.insert) H.empty
                    . Prelude.map (\s -> s { mag = magic s })
                    . Prelude.filter (isValidHash . hashA . el )
                    . concatMap go $ dirs
  where dirs = getDirections state
        moves = getMoves $ (M.!) (el state) (fl state)
        go :: Int -> [State]
        go dir = Prelude.map (\m ->
                      State
                      { fl = dir
                      , len = len state + 1,
                      el = M.adjust (flip S.difference m) (fl state) . M.adjust (S.union m) dir $ (el state) }
                      ) moves
res :: Maybe [State]
res = aStar nbrs dstA magic goalA initState

main :: IO()
main = do
  putStrLn $ "Part 1: " <> show (part1 initState)
  -- putStrLn $ "Part 2: " <> show (part2)
