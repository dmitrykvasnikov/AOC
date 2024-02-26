{-# LANGUAGE OverloadedStrings, ImportQualifiedPost #-}

module Main where

import Data.Map.Strict qualified as M
import Control.Applicative (Alternative, asum, many)
import Text.Regex.Applicative (RE, string, some, match, (<|>))
import Text.Regex.Applicative.Common (decimal)
import Data.Foldable (foldl')

data Action = On | Off | Toggle deriving (Show)
type Coord = [Int]
data Instruction = Instruction { action :: Action, start, stop :: Coord } deriving (Show)
type Input = [Instruction]
type Parser a = RE Char a
type State = Bool
type Area = M.Map Coord State
type Area2 = M.Map Coord Int

actionP :: Parser Action
actionP = (On <$ string "turn on ") <|> (Off <$ string "turn off ") <|> (Toggle <$ string "toggle ")

instructionP :: Parser Instruction
instructionP = Instruction <$> actionP <*> coordP <* (string " through ") <*> coordP

sepBy :: Alternative f => f a1 -> f a2 -> f [a1]
sepBy p sep = (:) <$> p <*> many (sep *> p)

coordP :: Parser Coord
coordP = decimal `sepBy` (string ",")

execInstruction :: Area -> Instruction -> Area
execInstruction area inst =
  let [x1, y1] = start inst
      [x2, y2] = stop inst
      coords = [[x,y] | x <- [x1..x2], y <- [y1..y2]]
      folder :: Area -> Coord -> Area
      folder area' coord' =
        case action inst of
          On -> M.insertWith (\_ _ -> True) coord' True area'
          Off -> M.insertWith (\_ _ -> False) coord' False area'
          Toggle -> M.insertWith (\_ v -> not v) coord' True area'
  in foldl' folder area coords

execInstruction2 :: Area2 -> Instruction -> Area2
execInstruction2 area inst =
  let [x1, y1] = start inst
      [x2, y2] = stop inst
      coords = [[x,y] | x <- [x1..x2], y <- [y1..y2]]
      folder :: Area2 -> Coord -> Area2
      folder area' coord' =
        case action inst of
          On -> M.insertWith (\n o -> n + o) coord' 1 area'
          Off -> M.insertWith (\_ o -> if o > 1 then o - 1 else 0) coord' 0 area'
          Toggle -> M.insertWith (\n o -> n + o) coord' 2 area'
  in foldl' folder area coords

getInput :: FilePath -> IO Input
getInput filepath = do
  i <- mapM (match instructionP) . lines <$> readFile filepath
  case i of
    Just r -> return r
    Nothing -> error "Wrong input file"

part1 :: IO ()
part1 = do
  input <- getInput "./app/input.txt"
  let res =  M.foldl' (\r v -> if v then r + 1 else r) 0 $ foldl' execInstruction M.empty input
  putStrLn $ show res

part2 :: IO ()
part2 = do
  input <- getInput "./app/input.txt"
  let res =  M.foldl' (\r v -> r + v) 0 $ foldl' execInstruction2 M.empty input
  putStrLn $ show res

main :: IO ()
main = putStrLn "Hello, Haskell!"
