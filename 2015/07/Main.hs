{-# LANGUAGE ApplicativeDo #-}


module Main where

import Control.Applicative (some, many, (<|>))
import Data.Maybe
import Data.Map qualified as M
import Control.Monad.State.Lazy
import Data.List
import Data.Bits qualified as B
import Text.Regex.Applicative (RE, match, string, sym, psym)
import Text.Regex.Applicative.Common (decimal)

type Input = [Command]

data Value = Num Int | Var String deriving (Show, Eq, Ord)
data Command = Let [Value] String | Not [Value] String | And [Value] String | Or [Value] String | Lshift [Value] String | Rshift [Value] String deriving (Show)

type Memory = M.Map Value Int

type Parser a = RE Char a

spaces :: Parser String
spaces = many $ psym (==' ')

valueP :: Parser Value
valueP = Num <$> decimal <|> Var <$> (some $ psym (flip elem ['a'..'z']))

resultP :: Parser String
resultP = spaces *> string "->" *> spaces *> (some $ psym (flip elem ['a'..'z']))

commandP :: Parser Command
commandP = letP  <|> notP <|> andP <|> orP <|> lshiftP <|> rshiftP

letP :: Parser Command
letP = do
  v <- valueP
  spaces
  res <- resultP
  return $ Let [v] res

notP :: Parser Command
notP = do
  string "NOT"
  spaces
  v <- valueP
  res <- resultP
  return $ Not [v] res

andP :: Parser Command
andP = do
  op1 <- valueP
  spaces
  string "AND"
  spaces
  op2 <- valueP
  res <- resultP
  return $ And [op1, op2] res

orP :: Parser Command
orP = do
  op1 <- valueP
  spaces
  string "OR"
  spaces
  op2 <- valueP
  res <- resultP
  return $ Or [op1, op2] res

lshiftP :: Parser Command
lshiftP = do
  op1 <- valueP
  spaces
  string "LSHIFT"
  spaces
  op2 <- valueP
  res <- resultP
  return $ Lshift [op1, op2] res

rshiftP :: Parser Command
rshiftP = do
  op1 <- valueP
  spaces
  string "RSHIFT"
  spaces
  op2 <- valueP
  res <- resultP
  return $ Rshift [op1, op2] res


filterIfNum :: Value -> Bool
filterIfNum arg = case arg of
                   Var _ -> True
                   Num _ -> False

isArgsKnown :: Memory -> Command -> Bool
isArgsKnown calculated cmd=
  let args = case cmd of
              Let v _ -> v
              Not v _ -> v
              Lshift v _ -> v
              Rshift v _ -> v
              And v _ -> v
              Or v _ -> v
      args' = filter filterIfNum args
  in
    case null args' of
      True -> True
      False -> all (flip M.member calculated) args'

getValue :: Value -> Memory -> Int
getValue v mem =
  case v of
    Num n -> n
    Var n -> fromJust $ M.lookup v mem


eval :: Memory -> Command -> Memory
eval memory (Let [v] r) = case (M.lookup (Var r)  memory) of
                            Nothing -> M.insert (Var r) (getValue v memory) memory
                            (Just _) -> memory
eval memory (Not [v] r) = M.insert (Var r) (B.complement $ getValue v memory) memory
eval memory (And [v1,v2] r) = M.insert (Var r) ((getValue v1 memory) B..&. (getValue v2 memory)) memory
eval memory (Or [v1,v2] r) = M.insert (Var r) ((getValue v1 memory) B..|. (getValue v2 memory)) memory
eval memory (Rshift [v1,v2] r) = M.insert (Var r) (B.shiftR (getValue v1 memory) (getValue v2 memory)) memory
eval memory (Lshift [v1,v2] r) = M.insert (Var r) (B.shiftL (getValue v1 memory) (getValue v2 memory)) memory

solve :: Memory -> Input -> Int
solve memory [] = fromJust $ M.lookup (Var "a") memory
solve memory input =
  let
    known = filter (isArgsKnown memory) input
    rest = filter (not. isArgsKnown memory) input
    memory' = foldl' eval memory known
  in solve memory' rest


getInput :: FilePath -> IO Input
getInput filepath = do
  input <- map (fromJust . match commandP) <$> lines <$> readFile filepath
  return input


part1 :: Input -> Int
part1 = solve M.empty

part2 :: Input -> Int
part2 = solve (M.fromList [(Var "b", 16076)])

main :: IO()
main = do
  input <- getInput "./input.txt"
--   input2 <- getInput "./input2.txt"
  putStrLn $ "Part 1: " <> show (part1 input)
  putStrLn $ "Part 2: " <> show (part2 input)
