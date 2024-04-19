module Main where

import Data.Maybe (fromMaybe, catMaybes)
import Text.Regex.Applicative (RE, match, (=~), sym, psym, string, many, some, (<|>))
import Text.Regex.Applicative.Common (decimal)
import Crypto.Hash
import Data.ByteString.Char8 (pack)
import Data.Char (toLower)
import Data.List (sort, sortOn)

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

data Direction = U | D | L | R deriving Show
type Path = String
type Input = String
type Coord = (Int, Int)
type State = (Coord, Path)

validDir :: Coord -> Direction -> Bool
validDir (_, 1) U = False
validDir (1, _) L = False
validDir (_, 4) D = False
validDir (4, _) R = False
validDir _ _      = True

move :: Coord -> Direction -> Coord
move (x,y) U = (x, y-1)
move (x,y) D = (x, y+1)
move (x,y) L = (x-1, y)
move (x,y) R = (x+1, y)

getDirections :: Coord -> Input -> Path -> [Direction]
getDirections c i p = let h = take 4 $ md5 $ i <> p
                    in filter (validDir c) . map snd . filter (flip elem ("bcdef" :: String) . fst) . zip h $ [U,D,L,R]

md5 :: String -> String
md5 bs = map toLower $ show ((hash . pack $ bs) :: Digest MD5)

dp :: State -> Input -> [Path]
dp ((4,4), path) _ = [path]
dp (c, p) i =
  let dirs = getDirections c i p
  in case null dirs of
    True -> []
    False -> concatMap go dirs
              where go d = dp (move c d, p ++ show d) i

getInput :: FilePath -> IO Input
getInput filepath = do
  input <- prepare <$> readFile filepath
  return input

prepare :: String -> Input
prepare = undefined

part1 :: Input -> String
part1  = head .  sortOn length . dp ((1,1), "")

part2 :: Input -> Int
part2  = length . head . reverse . sortOn length . dp ((1,1), "")

main :: IO()
main = do
  let input = "veumntbg"
  putStrLn $ "Part 1: " <> show (part1 input)
  putStrLn $ "Part 2: " <> show (part2 input)
