module Main where

import Data.Char (isSpace)
import Data.Maybe (fromMaybe, catMaybes)
import Text.Regex.Applicative (RE, match, (=~), sym, psym, string, many, some, (<|>), anySym)
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

mark :: Parser (Int, Int)
mark = (,) <$ sym '(' <*> decimal <* sym 'x' <*> decimal <* sym ')' <* some anySym

markLn :: (Int, Int) -> Int
markLn (l,r) = 3 + (length . show $ l) + (length . show $ r)

unz :: (String, Int) -> String
unz (str, cur)
  | cur >= length str = str
  | otherwise = case match mark (drop cur str) of
    Nothing -> unz (str, cur + 1)
    Just m@(l,r) -> let skip = markLn m
                        seg = take l $ drop (cur + skip) str
                        str' = take cur str ++ concat (take (r-1) $ repeat seg) ++ drop (cur + skip) str
                    in unz (str', cur + r*l )

unz2 :: String -> Int
unz2 str = 0 + go str
  where go :: String -> Int
        go [] = 0
        go s@(x:xs) = case match mark s of
          Nothing -> 1 + go xs
          Just m@(l,r) -> r * unz2 (take l $ drop mln s) + go (drop (mln + l) s)
            where mln = markLn m


type Input = String

getInput :: FilePath -> IO Input
getInput filepath = do
  input <- prepare <$> readFile filepath
  return input

prepare :: String -> Input
prepare = head . lines

part1 :: Input -> Int
part1 input = length $ unz (input, 0)

part2 :: Input -> Int
part2 = unz2

main :: IO()
main = do
  input <- getInput "./input.txt"
  putStrLn $ "Part 1: " <> show (part1 input)
  putStrLn $ "Part 2: " <> show (part2 input)
