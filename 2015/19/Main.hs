module Main where

import Data.Char (isAlpha, isLower)
import Data.List (foldl', isPrefixOf)
import Data.Set qualified as S
import Data.Maybe (fromJust)
import Data.Map.Strict qualified as M
import Text.Regex.Applicative (RE, match, psym, some, sym, string)

type Key = String
type Value = String
type Molecule = String
type Dictionary = M.Map Key [Value]
type Input = (Dictionary, Molecule)
type Parser a = RE Char a

keyval :: Parser (Key, Value)
keyval = (,) <$> (some $ psym isAlpha) <* string " => " <*> (some $ psym isAlpha) <* sym '\n'

input :: Parser ([(Key, Value)], Molecule)
input = (,) <$> some keyval <* sym '\n' <*> some (psym isAlpha) <* sym '\n'

mkDictionary :: [(Key, Value)] -> Dictionary
mkDictionary = foldl' (\m (k,v) -> M.insertWith (\n o -> n ++ o) k [v] m) M.empty

mkInput :: String -> Input
mkInput inp = let (d, s) = fromJust $ match input inp
              in (mkDictionary d, s)

getInput :: FilePath -> IO Input
getInput filepath = do
  input <- mkInput <$> readFile filepath
  return input

part1 :: Input -> Int
part1 (d, m) =
 S.size $ foldl' step S.empty [0..(length m - 1)]
  where step s i =
         case filter (flip isPrefixOf (drop i m)) (M.keys d) of
          [] -> s
          k  -> let prefixes = concatMap (\key -> zip (repeat key) (fromJust $ M.lookup key d)) k
                    molecules :: S.Set Molecule
                    molecules = foldl' add S.empty prefixes
                    add s (k, v) = S.insert (take i m <> v <> drop (i + length k) m) s
                in S.union s molecules

getMolecule :: Molecule -> (String, Molecule)
getMolecule [] = ("","")
getMolecule [x] = ([x], "")
getMolecule xs = let l = length $ takeWhile isLower (tail xs)
                 in (take (l + 1) xs, drop (l + 1) xs)

calc :: Molecule -> Int
calc "" = 0
calc xs = let (_, rest) = getMolecule xs
          in 1 + calc xs

part2 :: Input -> Int
part2 (d,m) = (go m 0) - 1
  where go [] n = n
        go mol n = let (mol', rest) = getMolecule mol
                   in case mol' of
                    "Rn" -> go rest n
                    "Ar" -> go rest n
                    "Y"  -> go rest (n - 1)
                    otherwise -> go rest (n+1)
main :: IO()
main = do
  input <- getInput "input.txt"
  putStrLn $ "Part 1: " <> show (part1 input)
  putStrLn $ "Part 2: " <> show (part2 input)
