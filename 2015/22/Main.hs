module Main where

import Data.Map.Strict qualified as M
import Data.Maybe (fromJust)
import Data.List (sort)

data Player = Player { pm, ph, pa :: Int}
data Boss = Boss { bh, bd :: Int}
data Spell = Missile | Drain | Shield | Poison | Recharge deriving (Eq, Ord)

type Spells = M.Map Spell Int

data Game = Game {player :: Player, boss :: Boss, spells :: Spells, turn :: Turn, cost :: Int}

data Turn = B | P deriving (Eq, Show)
next :: Turn -> Turn
next B = P
next P = B

costs :: [(Spell, Int)]
costs = [ (Missile, 53)
        , (Drain, 73)
        , (Shield, 113)
        , (Poison, 173)
        , (Recharge, 229)
        ]

cast :: Spell -> Game -> Game
cast Missile game = game { boss = (boss game) { bh = bh (boss game) - 4 }}
cast Drain game = game
                  { boss = (boss game) { bh = bh (boss game) - 2 }
                  , player = (player game) { ph = ph (player game) + 2 }}
cast Shield game = game { spells = M.insertWith (+) Shield 6 $ spells game }
cast Poison game = game { spells = M.insertWith (+) Poison 6 $ spells game }
cast Recharge game = game { spells = M.insertWith (+) Recharge 5 $ spells game }

evalGame :: Game -> Int -> [Int]
evalGame (Game p b s t c) d = case t of
  P | bh b <= 0 -> [c]
    | ph player' <= 0 -> [-1]
    | otherwise ->
      let possibleSpells = filter (\sp -> (snd sp <= pm p) && (not $ M.member (fst sp) s)) costs
      in if null possibleSpells then evalGame (Game player'' boss' spells' (next t) c) d
         else concatMap go $ map fst possibleSpells where
           go sp = evalGame (cast sp $ Game { player = player'' {pm = pm player'' - cost'}, boss = boss', spells =  spells', turn = (next t), cost = (c + cost') }) d
            where cost' = fromJust . lookup sp $ costs
  B -> evalGame (Game { player = player'' { ph = ph player'' - max 1 (bd b - armor) }, boss = boss', spells = spells', turn = next t, cost = c }) d
  where
    spells' = M.filter (>0) $ subtract 1 <$> s
    boss' = foldr castBoss b $ M.keys s
    player' = if t == B then p else p { ph = ph p - d }
    player'' = foldr castPlayer player' $ M.keys s
    armor = if M.member Shield s then 7 else 0

castBoss :: Spell -> Boss -> Boss
castBoss Poison b = b { bh = bh b - 3 }
castBoss _ b      = b

castPlayer :: Spell -> Player -> Player
castPlayer Recharge p = p { pm = pm p + 101 }
castPlayer _ p        = p

type Input = String

getInput :: FilePath -> IO Input
getInput filepath = do
  input <- readFile filepath
  return input

part1 :: Int
part1 = head . sort . filter (/=(-1)) . evalGame  (Game (Player 500 50 0) (Boss 51 9) M.empty P 0) $ 0

part2 :: Int
part2 = head . sort . filter (/=(-1)) . evalGame  (Game (Player 500 50 0) (Boss 51 9) M.empty P 0) $ 1

main :: IO()
main = do
  input <- getInput "./input.txt"
  putStrLn $ "Part 1: " <> show (part1)
  putStrLn $ "Part 2: " <> show (part2)
