module Main where

import AOC
import Data.Maybe (fromMaybe, catMaybes, fromJust)
import Data.List (sortOn)
import Data.Char (isAlpha)
import Data.Map.Strict qualified as M
import Text.Regex.Applicative (RE, match, (=~), sym, psym, string, many, some, (<|>), empty, anySym)
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

data Props = Props { _w :: Int, _t :: [String] } deriving Show
type Towers = M.Map String Props

type Input = Towers

str :: Parser String
str = some (psym isAlpha)

tower :: Parser (String, Props)
tower = (,) <$> str <*> props

props :: Parser Props
props = Props <$ string " (" <*> decimal <* sym ')'  <*> (string " -> " *> sepBy str (string ", ")  <|> mempty)

prepare :: String -> Input
prepare = M.fromList . catMaybes . map (match tower) . lines

getTowers :: Towers -> String -> Int
getTowers ts t = let ps = fromJust . M.lookup t $ ts
                     ts' = _t ps
                  in case null $ ts' of
                       True -> 0
                       False -> length ts' + (sum . map (getTowers ts) $ ts')

getWeight :: Towers -> [String] -> Int
getWeight _ [] = 0
getWeight ts (x:xs) = let ps = fromJust . M.lookup x $ ts
                      in _w ps + getWeight ts (xs ++ _t ps)

getBottom :: Input -> String
getBottom i =  snd . head . reverse . sortOn fst . map (\t -> (getTowers i t, t)) . map fst . M.toList $ i

getWeights :: Input -> String -> [(String, Int)]
getWeights i t = let ts = t' i t
                    in map (\t'' -> (t'', getWeight i [t''])) ts

getCand :: [(String, Int)] -> Maybe (String, Int)
getCand cs = let ws = map snd cs
                 mn = minimum ws
                 mx = maximum ws
                 mins = (==1) . length . filter (== mn) $ ws
                 minT = fst . head . filter (\(_, w) -> w == mn) $ cs
                 maxT = fst . head . filter (\(_, w) -> w == mx) $ cs
             in case mn == mx of
                  True -> Nothing
                  False -> if mins then Just (minT, mx - mn) else Just (maxT, mn - mx)

getUnbalanced :: Towers -> String -> Int -> (String, Int)
getUnbalanced ts t d = case getCand . getWeights ts $ t of
                         Nothing -> (t, (w' ts t) + d)
                         Just (t', d') -> getUnbalanced ts t' d'

w' :: Towers -> String -> Int
w' ts t = _w . fromJust . M.lookup t $ ts
t' :: Towers -> String -> [String]
t' ts t = _t . fromJust . M.lookup t $ ts

part1 :: Input -> String
part1 = getBottom

part2 :: Input -> (String, Int)
part2 i = getUnbalanced i (getBottom i) 0

main :: IO()
main = do
  input <- prepare <$> readFile "./input.txt"
  putStrLn $ "Part 1: " <> show (part1 input)
  putStrLn $ "Part 2: " <> show (part2 input)
