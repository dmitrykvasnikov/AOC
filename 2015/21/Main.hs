module Main where

import Data.List (sort)

type Input = String

data Result = Win | Lose | Continue deriving (Eq, Show)
data Player = Player { hit, armor, damage :: Int } deriving Show
data Equipment = Equipment { c, d, a :: Int } deriving Show

instance Semigroup Equipment where
 Equipment c1 a1 d1 <> Equipment c2 a2 d2 = Equipment (c1 + c2) (a1 + a2) (d1 + d2)

instance Monoid Equipment where
 mempty = Equipment 0 0 0

boss :: Player
boss = Player 104 1 8

-- /** Equipment
weapons, armors, rings :: [Equipment]
weapons = [Equipment 8 4 0, Equipment 10 5 0, Equipment 25 6 0, Equipment 40 7 0, Equipment 74 8 0]
armors = [Equipment 13 0 1, Equipment 31 0 2, Equipment 53 0 3, Equipment 75 0 4, Equipment 102 0 5]
rings = [Equipment 25 1 0, Equipment 50 2 0, Equipment 100 3 0, Equipment 20 0 1, Equipment 40 0 2, Equipment 80 0 3]
-- **/

allRings :: [Equipment] -> [Equipment]
allRings [] = []
allRings (x:xs) = [x] ++ (map (\x' -> mconcat [x,x']) xs) ++ allRings xs

allWA :: [Equipment] -> [Equipment] -> [Equipment]
allWA [] _ = []
allWA (w:ws) as = [w] ++ (map (\a -> mconcat [w,a]) as) ++ allWA ws as

allWAR :: [Equipment] -> [Equipment] -> [Equipment] -> [Equipment]
allWAR w a r = let wa = allWA w a
               in allWA wa (allRings r)

war :: [Equipment]
war = allWAR weapons armors rings

check :: Int -> Int
check d
  | d >= 0 = -1
  | otherwise = d

result :: (Player, Player) -> Result
result (p1, p2)
  | hit p2 <= 0 = Win
  | hit p1 <= 0 = Lose
  | otherwise = Continue

step :: (Player, Player) -> (Player, Player)
step (p1, p2) =
  let d1 = check $ armor p1 - damage p2
      d2 = check $ armor p2 - damage p1
  in (p1 { hit = (hit p1 + d1) }, p2 { hit = hit p2 + d2 })


game :: (Player, Player) -> Result
game = head . dropWhile (== Continue) . map result . iterate step

evalGame :: Result -> Int -> Player -> Player -> Int
evalGame res c p1 p2 =
  case game (p1, p2) == res of
    True-> c
    False -> -1

getInput :: FilePath -> IO Input
getInput filepath = do
  input <- readFile filepath
  return input

part1 :: [Equipment] -> Int
part1 = head . sort . filter (/=(-1)) . map go
  where
    go war' = evalGame Win (c war') (Player 100 (a war') (d war')) boss

part2 :: [Equipment] -> Int
part2 = head . reverse . sort . filter (/=(-1)) . map go
  where
    go war' = evalGame Lose (c war') (Player 100 (a war') (d war')) boss

main :: IO()
main = do
--   input <- getInput "./input.txt"
  putStrLn $ "Part 1: " <> show (part1 war)
  putStrLn $ "Part 2: " <> show (part2 war)
