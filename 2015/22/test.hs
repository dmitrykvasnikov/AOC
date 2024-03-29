{-# LANGUAGE FlexibleContexts, NamedFieldPuns, RecordWildCards, ViewPatterns #-}
module Day22 (day22a, day22b) where

import Control.Monad.State (MonadState, evalState, gets, modify)
import Data.Heap (MinPrioHeap)
import qualified Data.Heap as Heap (insert, singleton, view)
import Data.List (find, foldl', sortBy)
import Data.Map (Map)
import qualified Data.Map as Map (empty, filter, insertWith, keys, lookup, member)
import Data.Ord (Down(..), comparing)
import Data.Set (Set)
import qualified Data.Set as Set (empty, insert, member)
import Text.Parsec (ParseError, ParsecT, eof, many1, optional, parse)
import Text.Parsec.Char (digit, newline, string)
import Text.Read (readMaybe)

data Spell = MagicMissile | Drain | Shield | Poison | Recharge
  deriving (Bounded, Eq, Ord, Show)
data Player a = Player { playerHp :: a, mana :: a }
  deriving (Eq, Ord, Show)
data Boss a = Boss { bossHp :: a, atk :: a } deriving (Eq, Ord, Show)
data Game a
  = PlayerTurn { player :: Player a, boss :: Boss a, active :: Map Spell Int }
  | BossTurn { player :: Player a, boss :: Boss a, active :: Map Spell Int }
  deriving (Eq, Ord, Show)

rpg :: (Monad m, Read a) => ParsecT String u m (Boss a)
rpg = do
    Just hp <- fmap readMaybe $ string "Hit Points: " *> many1 digit <* newline
    Just atk <- fmap readMaybe $ string "Damage: " *> many1 digit <* newline
    return $ Boss hp atk

costs :: (Num a) => [(Spell, a)]
costs =
  [ (MagicMissile, 53)
  , (Drain, 73)
  , (Shield, 113)
  , (Poison, 173)
  , (Recharge, 229)
  ]

cast :: (Num a) => Spell -> Game a -> Game a
cast MagicMissile game =
    game {boss = (boss game) {bossHp = bossHp (boss game)- 4}}
cast Drain game = game
  { player = (player game) {playerHp = playerHp (player game) + 2}
  , boss = (boss game) {bossHp = bossHp (boss game) - 2}
  }
cast Shield game = game {active = Map.insertWith (+) Shield 6 $ active game}
cast Poison game = game {active = Map.insertWith (+) Poison 6 $ active game}
cast Recharge game = game {active = Map.insertWith (+) Recharge 5 $ active game}

affectPlayer :: (Num a) => Spell -> Player a -> Player a
affectPlayer Recharge player = player {mana = mana player + 101}
affectPlayer _ player = player

affectBoss :: (Num a) => Spell -> Boss a -> Boss a
affectBoss Poison boss = boss {bossHp = bossHp boss - 3}
affectBoss _ boss = boss

play :: (Num a, Ord a, MonadState (Set (Game a)) m) =>
    a -> MinPrioHeap a (Game a) -> m (Maybe a)
play ouch (Heap.view -> Just ((total, game), heap)) = do
    seen <- gets $ Set.member game
    if seen then play ouch heap else case game of
        _ | bossHp (boss game) <= 0 -> return $ Just total
        _ | playerHp player' <= 0 -> play ouch heap
        PlayerTurn {} -> total `seq` play ouch $ foldr Heap.insert heap
          [ (total + cost, cast spell game')
          | (spell, cost) <- costs
          , not $ Map.member spell active'
          , cost <= mana player''
          , let game' = BossTurn
                  { player = player'' {mana = mana player'' - cost}
                  , boss = boss'
                  , active = active'
                  }
          ]
        BossTurn {} -> play ouch $ Heap.insert
          ( total
          , PlayerTurn
              { player = player''
                    {playerHp = playerHp player'' - max 1 (atk boss' - armor)}
              , boss = boss'
              , active = active'
              }
          ) heap
  where
    player'
      | PlayerTurn {player} <- game = player {playerHp = playerHp player - ouch}
      | BossTurn {player} <- game = player
    player'' = foldr affectPlayer player' . Map.keys $ active game
    boss' = foldr affectBoss (boss game) . Map.keys $ active game
    active' = Map.filter (> 0) $ subtract 1 <$> active game
    armor = if Map.member Shield $ active game then 7 else 0
play _ _ = return Nothing

day22a :: String -> Either ParseError (Maybe Int)
day22a input = do
    game <- PlayerTurn (Player 50 500) `flip` Map.empty <$> parse rpg "" input
    return . flip evalState Set.empty . play 0 $ Heap.singleton (0, game)

day22b :: String -> Either ParseError (Maybe Int)
day22b input = do
    game <- PlayerTurn (Player 50 500) `flip` Map.empty <$> parse rpg "" input
    return . flip evalState Set.empty . play 1 $ Heap.singleton (0, game)
