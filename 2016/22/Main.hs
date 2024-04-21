module Main where

import Data.Maybe (fromMaybe, fromJust, catMaybes)
import Data.Char (isDigit)
import Text.Regex.Applicative (RE, match, (=~), sym, psym, string, many, some, (<|>), anySym)
import Text.Regex.Applicative.Common (decimal)
import Data.Map.Strict qualified as M

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

type Input = FS
type Size = Int
type Used = Int
type Node = (Size, Used)
type Coord = (Int, Int)
type FS = M.Map Coord Node

getInput :: FilePath -> IO Input
getInput filepath = do
  input <- prepare <$> readFile filepath
  return input

skip :: Parser String
skip = many (psym (not . isDigit))

prepare :: String -> Input
prepare = M.fromList . catMaybes . map (match parse) . drop 2 . lines
  where parse = (,) <$> coord <*> node
        coord = (,) <$ skip <*> decimal <* string "-y" <*> decimal
        node = (,) <$ skip <*> decimal <* skip <*> decimal <* many anySym

viable :: FS -> Int
viable fs = go coords
  where coords = M.keys fs
        go :: [Coord] -> Int
        go [] = 0
        go [x] = 0
        go (x:xs) = (length . filter (== True) . concatMap (check x) $ xs) + go xs
        check :: Coord -> Coord -> [Bool]
        check c1 c2 = [isViable c1 c2, isViable c2 c1]
        isViable :: Coord -> Coord -> Bool
        isViable c1 c2 =
          let (s1, u1) = fromJust . M.lookup c1 $ fs
              (s2, u2) = fromJust . M.lookup c2 $ fs
          in u1 /= 0 && u1 <= (s2 - u2)

freeCell :: Input -> Int
freeCell = fst . snd . head . M.toList . M.filter ( (== 0) . snd )


printFS :: FS -> [String]
printFS fs = map getRow [0..29]
  where fc = freeCell fs
        getRow y = map getChar [0..31]
          where getChar x = let (s, u) = fromJust . M.lookup (x,y) $ fs
                            in case s <= fc of
                                True -> if u == 0 then '*' else '.'
                                False -> '#'

part1 :: Input -> Int
part1 = viable

-- answer to part 2 is pen / paper calculation
-- answer is 195
part2 :: Input -> IO()
part2 input = do
  mapM_ putStrLn $ printFS input


main :: IO()
main = do
  input <- getInput "./input.txt"
  -- putStrLn $ "Part 1: " <> show (part1 input)
  putStrLn $ "Part 2: "
  part2 input
