module Main where

import Text.Regex.Applicative (RE, sym, psym, string, (<|>), match)
import Text.Regex.Applicative.Common (decimal)
import Data.Map.Strict qualified as M
import Data.Maybe (catMaybes, fromJust)

data Register = A | B deriving (Show, Eq, Ord)
data Instruction = HLF Register | TPL Register | INC Register | JMP Int | JIE Register Int | JIO Register Int deriving (Show, Eq)
data Status = Halt | Running deriving (Show, Eq)
type Memory = M.Map Register Int
type Program = [Instruction]
data PC = PC { ptr :: Int, program :: Program, memory :: Memory, status :: Status } deriving (Show, Eq)

emptyMemoryP1, emptyMemoryP2 :: Memory
emptyMemoryP1 = M.fromList [(A, 0), (B, 0)]
emptyMemoryP2 = M.fromList [(A, 1), (B, 0)]

type Input = [Instruction]

type Parser a = RE Char a

register :: Parser Register
register = (A <$ sym 'a') <|> (B <$ sym 'b')

inc, hlf, tpl, jmp, jie, jio :: Parser Instruction
inc = INC <$ string "inc " <*> register
hlf = HLF <$ string "hlf " <*> register
tpl = TPL <$ string "tpl " <*> register
jmp = JMP <$ string "jmp " <*> ((sym '+' *> decimal) <|> (sym '-' *> fmap (*(-1)) decimal))
jio = JIO <$ string "jio " <*> register <* string ", " <*> ((sym '+' *> decimal) <|> (sym '-' *> fmap (*(-1)) decimal))
jie = JIE <$ string "jie " <*> register <* string ", " <*> ((sym '+' *> decimal) <|> (sym '-' *> fmap (*(-1)) decimal))

instruction :: Parser Instruction
instruction = inc <|> hlf <|> tpl <|> jmp <|> jio <|> jie

tick :: PC -> PC
tick pc@(PC p pr m s)
  | s == Halt = pc
  | p < 1 || p > length pr = pc { status = Halt }
  | otherwise = case  pr !! (p - 1) of
                  INC r -> pc { ptr = p + 1, memory = M.adjust (+1) r m }
                  TPL r -> pc { ptr = p + 1, memory = M.adjust (*3) r m }
                  HLF r -> pc { ptr = p + 1, memory = M.adjust (flip div 2) r m }
                  JMP j -> pc { ptr = p + j }
                  JIO r j -> if 1 == (fromJust . M.lookup r $ m) then pc { ptr = p + j }
                                                                else pc { ptr = p + 1 }
                  JIE r j -> if even (fromJust . M.lookup r $ m) then pc { ptr = p + j }
                                                                else pc { ptr = p + 1 }

getInput :: FilePath -> IO Input
getInput filepath = do
  input <- catMaybes . map (match instruction) . lines <$> readFile filepath
  return input

isRunning :: PC -> Bool
isRunning pc = status pc == Running

part1 :: Input -> Int
part1 program = fromJust . M.lookup B . memory . head . dropWhile isRunning . iterate tick $ PC 1 program emptyMemoryP1 Running

part2 :: Input -> Int
part2 program = fromJust . M.lookup B . memory . head . dropWhile isRunning . iterate tick $ PC 1 program emptyMemoryP2 Running

main :: IO()
main = do
  input <- getInput "./input.txt"
  putStrLn $ "Part 1: " <> show (part1 input)
  putStrLn $ "Part 2: " <> show (part2 input)
