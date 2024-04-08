module Main where

import Data.Maybe (catMaybes)
import Text.Regex.Applicative (RE, match, (=~), sym, psym, string, many, some, (<|>))
import Data.Char (isAlpha)
import Data.List (isInfixOf)

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

type Abbr = String
data Type = S | H deriving (Show, Eq)
data Segment = Segment Type Abbr deriving Show
type Input = [[Segment]]

segment :: Parser Segment
segment = (Segment S <$> some (psym isAlpha)) <|> (Segment H <$ sym '[' <*> some (psym isAlpha) <* sym ']')

isABBA :: Segment -> Bool
isABBA (Segment t a) = case (length a < 4) of
              True -> False
              False -> ((head a) /= (head . tail $ a) &&  (take 2 a) == reverse (take 2 (drop 2 a))) || isABBA (Segment t (tail a))

isValid, isValid' :: [Segment] -> Bool
isValid segs = (not $ any isABBA hs) && (any isABBA ss)
                    where hs = filter (\(Segment t _) -> t == H) segs
                          ss = filter (\(Segment t _) -> t == S) segs
isValid' segs = any (isABA hs) ss
                    where hs = filter (\(Segment t _) -> t == H) segs
                          ss = filter (\(Segment t _) -> t == S) segs

isBAB :: Abbr -> [Segment] -> Bool
isBAB abbr = any (\(Segment _ a) -> isInfixOf abbr a)

isABA :: [Segment] -> Segment -> Bool
isABA hs (Segment t abbr) = case (length abbr < 3) of
    True -> False
    False -> let (x:y:z:_) = abbr
             in ((x == z) && (x /= y) && (isBAB (y:z:y:[])  hs)) || isABA hs (Segment t (tail abbr))

getInput :: FilePath -> IO Input
getInput filepath = do
  input <- prepare <$> readFile filepath
  return input

prepare :: String -> Input
prepare = catMaybes . map (match (some segment)) . lines

part1 :: Input -> Int
part1 = length . filter isValid

part2 :: Input -> Int
part2 = length . filter isValid'

main :: IO()
main = do
  input <- getInput "input.txt"
  putStrLn $ "Part 1: " <> show (part1 input)
  putStrLn $ "Part 2: " <> show (part2 input)
