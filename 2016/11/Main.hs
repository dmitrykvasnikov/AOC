module Main where
import Prelude hiding (init)
import Data.Maybe (fromMaybe, catMaybes)
import Text.Regex.Applicative (RE, match, (=~), sym, psym, string, many, some, (<|>))
import Text.Regex.Applicative.Common (decimal)
import Data.Set qualified as S
import Data.Map.Strict qualified as M
import Debug.Trace
import Data.List (sort, sortOn, intercalate)

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

data Equipment = G | M deriving (Eq, Ord, Show)
data Rock = T | Pr | S | Pl | R | E | D deriving (Eq, Ord, Show)
type Item = (Rock, Equipment)
type Floor = S.Set Item
type Hash = String
type Elevator = M.Map Int Floor
type States = M.Map Hash Int
data State = State {el :: Elevator, fl, len :: Int} deriving Show

init :: State
init = State { el = M.fromList [ (1, S.fromList [(D,G), (D,M), (E,G), (E,M), (T,G), (T,M), (Pl,G),(S,G)])
                               , (2, S.fromList [(Pl,M), (S,M)])
                               , (3, S.fromList [(Pr,G), (Pr,M), (R,G), (R,M)])
                               , (4, S.fromList [])], fl = 1, len = 0 }

showF :: Floor -> String
showF = concat . pairs . map show  . S.toList
  where pairs :: [String] -> [String]
        pairs [] = []
        pairs [x] = [x]
        pairs (s1@(_:x1:_):s2@(_:x2:_):rest)
         | x1 == x2 = "(P)":pairs rest
         | otherwise = s1 : pairs (s2:rest)

showE :: State -> String
showE st = show (fl st) <> (intercalate "|" .  map ( showF . snd) . M.toList . el $ st)

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
hashA = concat . map (hash1 . snd) . M.toList

isValidHash :: Hash -> Bool
isValidHash [] = True
isValidHash (x:y:z:rest) = ((y == '0') || (x == '0' && z == '0')) && isValidHash rest

isGoal :: Hash -> Bool
isGoal [x] = True
isGoal (x:xs) = x == '0' && isGoal xs

getStates :: State -> [State]
getStates state = filter (isValidHash . hashA . el) . concatMap go $ dirs
  where dirs = getDirections state
        moves = getMoves $ (M.!) (el state) (fl state)
        go :: Int -> [State]
        go dir = map (\m ->
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
getMoves = map S.fromList . filter isValidPair . go . S.toList
  where go [] = []
        go (x:xs) = [[x]] ++ map (:x:[]) xs ++ go xs
        isValidPair [x] = True
        isValidPair ((_,G):(_,G):_) = True
        isValidPair ((_,M):(_,M):_) = True
        isValidPair ((r1, _):(r2,_):_) = r1 == r2

elevator :: [State] -> States -> States
elevator [] states = states
elevator (s:ss) states
  | M.member h' states = elevator ss states
  | isGoal h = elevator ss $ M.insert h' (len s) states
  | otherwise =  elevator (ss ++ (getStates s)) $ M.insert h' (len s) states
  where h = hashA $ el s
        h' = showE s

part1 :: Int
part1 = head . reverse . map snd . M.toList . elevator [init] $ M.empty

part2 :: Int
part2 = flip (M.!) "000000000001" . elevator [init] $ M.empty

main :: IO()
main = do
  putStrLn $ "Part 1: " <> show (part1)
  -- putStrLn $ "Part 2: " <> show (part2)
