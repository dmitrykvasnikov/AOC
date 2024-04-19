module Main where

import Data.Maybe (fromMaybe, catMaybes)
import Text.Regex.Applicative (RE, match, (=~), sym, psym, string, many, some, (<|>))
import Text.Regex.Applicative.Common (decimal)

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

type Input = String

rep :: Char -> Char
rep '0' = '1'
rep '1' = '0'

rev :: String -> String
rev [] = []
rev s = map invert $ reverse s

invert :: Char -> Char
invert '1' = '0'
invert '0' = '1'

dragon :: Int -> String -> String
dragon l s
 | length s >= l = take l s
 | otherwise = dragon l (s ++ "0" ++ rev s)

checkSum :: String -> String
checkSum s = let res = go s
                 go :: String -> String
                 go [] = []
                 go (x:y:rest) = if x == y then '1' : go rest else '0' : go rest
             in case even $ length res of
                 True -> checkSum res
                 False -> res

getInput :: FilePath -> IO Input
getInput filepath = do
  input <- prepare <$> readFile filepath
  return input

prepare :: String -> Input
prepare = undefined

part1 :: Input -> String
part1 = checkSum . (dragon 272)

part2 :: Input -> String
part2 = checkSum . (dragon 35651584)

main :: IO()
main = do
  let input = "11110010111001001"
  putStrLn $ "Part 1: " <> show (part1 input)
  putStrLn $ "Part 2: " <> show (part2 input)
