module Main where

import AOC
import Prelude hiding (LT, GT)
import Data.Maybe (fromJust, fromMaybe, catMaybes)
import Data.List (sort, foldl')
import Data.Char (isAlpha)
import Data.Map.Strict qualified as M
import Text.Regex.Applicative (RE, match, (=~), sym, psym, string, many, some, (<|>))
import Text.Regex.Applicative.Common (decimal, signed)

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

data Op = Inc | Dec deriving (Show, Eq)
data Cmp = L | G | E | NE | LT | GT deriving (Show, Eq)
type Register = String
type Value = Int
type Memory = M.Map Register Value
data Instruction = Instruction Register Op Value Register Cmp Value deriving Show
data PC = PC { _m :: Memory, _v :: Int } deriving Show

type Input = [Instruction]

sp :: Parser Char
sp = sym ' '
regP :: Parser Register
regP = some (psym isAlpha)
opP :: Parser Op
opP = Inc <$ string "inc" <|> Dec <$ string "dec"
cmpP = L <$ string "<" <|> G <$ string ">" <|> E <$ string "==" <|> NE <$ string "!=" <|> LT <$ string "<=" <|> GT <$ string ">="
ins :: Parser Instruction
ins = Instruction <$> regP <* sp <*> opP <* sp <*> (signed decimal) <* string " if " <*> regP <* sp <*> cmpP <* sp <*> (signed decimal)


cmp :: Memory -> Cmp -> Register -> Value -> Bool
cmp m c r v = let v' = M.findWithDefault 0 r $ m
              in case c of
                   L -> v' < v
                   G -> v' > v
                   E -> v' == v
                   NE -> v' /= v
                   GT -> v' >= v
                   LT -> v' <= v

getBiggestRegister :: PC -> Int
getBiggestRegister = head . reverse . sort . map snd . M.toList . _m

runOp :: Op -> Value -> Value -> Value
runOp Inc v1 v2 = v1 + v2
runOp Dec v1 v2 = v1 - v2

op :: PC -> Op -> Register -> Value -> PC
op pc o r v = let oldV = M.findWithDefault 0 r (_m pc)
                  newV = runOp o oldV v
              in PC (M.insert r newV (_m pc)) (max (_v pc) newV)

step :: PC -> Instruction -> PC
step pc (Instruction r1 o v1 r2 c v2)
  | cmp (_m pc) c r2 v2 = op pc o r1 v1
  | otherwise     = pc

prepare :: String -> Input
prepare = catMaybes . map (match ins) . lines

part1 :: Input -> Int
part1 = getBiggestRegister . foldl' step (PC M.empty 0)

part2 :: Input -> Int
part2 = _v . foldl' step (PC M.empty 0)

main :: IO()
main = do
  input <- prepare <$> readFile "./input.txt"
  putStrLn $ "Part 1: " <> show (part1 input)
  putStrLn $ "Part 2: " <> show (part2 input)
