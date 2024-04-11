module Main where

import Data.Map.Strict qualified as M
import Data.Char (isAlpha)
import Data.Maybe (fromMaybe, catMaybes, fromJust)
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

data Register = A | B | C | D deriving (Show, Eq, Ord, Enum)
data Arg = Reg Register | Val Int deriving (Show, Eq, Ord)
data Instruction = CPY Arg Arg | INC Arg | DEC Arg | JNZ Arg Arg deriving (Show, Eq, Ord)
type Memory = M.Map Register Int
type Program = [Instruction]
data PC = PC { program :: Program, mem :: Memory, ptr :: Int , halt :: Bool}

type Input = Program

space :: Parser Char
space = sym ' '

arg :: Parser Arg
arg = Val <$> signed decimal
  <|> Reg A <$ sym 'a'
  <|> Reg B <$ sym 'b'
  <|> Reg C <$ sym 'c'
  <|> Reg D <$ sym 'd'

instruction :: Parser Instruction
instruction = CPY <$ string "cpy " <*> arg <* space <*> arg
          <|> INC <$ string "inc " <*> arg
          <|> DEC <$ string "dec " <*> arg
          <|> JNZ <$ string "jnz " <*> arg <* space <*> arg

eval :: Arg -> Memory -> Int
eval (Val n) _      = n
eval (Reg reg) mem  = fromJust $ M.lookup reg mem

step :: PC -> PC
step pc
  | halt pc = pc
  | length (program pc) < ptr pc = pc { halt = True }
  | otherwise = case program pc !! (ptr pc - 1) of
      INC (Reg reg) -> pc { ptr = ptr pc + 1, mem = M.adjust (+1) reg (mem pc) }
      DEC (Reg reg) -> pc { ptr = ptr pc + 1, mem = M.adjust (+(-1)) reg (mem pc) }
      CPY val (Reg reg) -> pc { ptr = ptr pc + 1, mem = M.adjust (const $ eval val (mem pc)) reg (mem pc) }
      JNZ reg  jump -> case (eval reg (mem pc)) /= 0 of
        False -> pc { ptr = ptr pc + 1 }
        True -> pc { ptr = ptr pc + eval jump (mem pc) }

getInput :: FilePath -> IO Input
getInput filepath = do
  input <- prepare <$> readFile filepath
  return input

prepare :: String -> Input
prepare = catMaybes . map (match instruction) . lines

part1 :: Input -> [Int] -> Int
part1 input m = fromJust . M.lookup A . mem . head .  filter halt . iterate step $ PC { halt = False, ptr = 1, program = input, mem = M.fromList (zip [A,B,C,D] m) }

part2 :: Input -> Int
part2 input = 42

main :: IO()
main = do
  input <- getInput "./input.txt"
  putStrLn $ "Part 1: " <> show (part1 input [0,0,0,0])
  putStrLn $ "Part 2: " <> show (part1 input [0,0,1,0])
