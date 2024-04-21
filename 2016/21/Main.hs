module Main where

import Data.Maybe (fromMaybe, catMaybes)
import Text.Regex.Applicative (RE, match, (=~), sym, psym, string, many, some, (<|>), anySym)
import Text.Regex.Applicative.Common (decimal)
import Data.Bits (Bits(xor))

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

data Op = SwapP Int Int | SwapL Char Char | RotateL Int | RotateR Int | Rotate Char | Unrotate Char | Reverse Int Int | Move Int Int deriving Show

type Input = [Op]

op :: Parser Op
op =   SwapP <$ string "swap position " <*> decimal <* string " with position " <*> decimal
   <|> SwapL <$ string "swap letter " <*> anySym <* string " with letter " <*> anySym
   <|> RotateL <$ string "rotate left " <*> decimal <* many anySym
   <|> RotateR <$ string "rotate right " <*> decimal <* many anySym
   <|> Rotate <$ string "rotate based on position of letter " <*> anySym
   <|> Reverse <$ string "reverse positions " <*> decimal <* string " through " <*> decimal
   <|> Move <$ string "move position " <*> decimal <* string " to position " <*> decimal

runOp :: String -> Op -> String
runOp i o =
  case o of
    (SwapP x y)   -> let a = i !! x in change y a . change x (i !! y) $ i
    (SwapL a b)   -> map (swap a b) i
    (RotateR x)   -> let x' = x `mod` len in drop (len - x') i ++ take (len - x') i
    (RotateL x)   -> let x' = x `mod` len in drop x' i ++ take x' i
    (Rotate c)    -> let ind = getIndex i c
                         ind' = if ind >= 4 then ind + 2 else ind + 1
                     in runOp i (RotateR ind')
    (Unrotate c)  -> let ind = getIndex i c
                         ind' = if ind == 0
                                then 1
                                else case odd ind of
                                      True -> (div ind 2) + 1
                                      False -> (div ind 2) + 5
                     in runOp i (RotateL ind')
    (Reverse x y) -> take x i ++ reverse (take (y - x + 1) . drop x $ i) ++ drop (y + 1) i
    (Move x y)    -> let i' = take x i ++ drop (x+1) i
                     in take y i' ++ [i !! x] ++ drop y i'
  where
    len = length i
    change :: Int -> Char -> String -> String
    change i c str = take i str ++ [c] ++ drop (i+1) str
    swap :: Char -> Char -> Char -> Char
    swap a b c
      | a == c = b
      | b == c = a
      | otherwise = c
    getIndex :: String -> Char -> Int
    getIndex [] _ =  0
    getIndex (x:xs) c
      | c == x = 0
      | otherwise = 1 + getIndex xs c


revOp :: Op -> Op
revOp (SwapP x y) = SwapP y x
revOp (SwapL a b) = SwapL a b
revOp (RotateR x) = RotateL x
revOp (RotateL x) = RotateR x
revOp r@(Reverse _ _) = r
revOp (Move x y)  = Move y x
revOp (Rotate c)  = Unrotate c

getInput :: FilePath -> IO Input
getInput filepath = do
  input <- prepare <$> readFile filepath
  return input

prepare :: String -> Input
prepare = catMaybes . map (match op) . lines

part1 :: Input -> String
part1 = foldl runOp "abcdefgh"

part2 :: Input -> String
part2 = foldl runOp "fbgdceah" . map revOp . reverse

main :: IO()
main = do
  input <- getInput "./input.txt"
  putStrLn $ "Part 1: " <> show (part1 input)
  putStrLn $ "Part 2: " <> show (part2 input)
