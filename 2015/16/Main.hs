module Main where

import Data.Char (isAlpha)
import Data.Maybe (mapMaybe, fromJust)
import Text.Regex.Applicative (RE, match, many, psym, string, some)
import Text.Regex.Applicative.Common (decimal)

sue :: Aunt
sue = [("children", 3)
       , ("cats", 7)
       , ("samoyeds", 2)
       , ("pomeranians", 3)
       , ("akitas", 0)
       , ("vizslas", 0)
       , ("goldfish", 5)
       , ("trees", 3)
       , ("cars", 2)
       , ("perfumes", 1)
       ]

type Aunt = [(String, Int)]
type Aunts = [(Int, Aunt)]
type Input = Aunts

type Parser a = RE Char a

prop :: Parser (String, Int)
prop = (,) <$> (some (psym isAlpha)) <* string ": " <*> decimal

aunt :: Parser (Int, [(String, Int)])
aunt = (,) <$ string "Sue " <*> decimal <* string ": "  <*> sepBy prop (string ", ")
  where sepBy p sep = (:) <$> p <*> many (sep *> p)

comp :: Aunt -> Bool
comp [] = True
comp ((s, v) : as) = fromJust (lookup s sue) == v && comp as

comp2 :: Aunt -> Bool
comp2 [] = True
comp2 ((s, v) : as) = case s of
                        "cats" -> (fromJust (lookup s sue) < v) && comp2 as
                        "trees" -> (fromJust (lookup s sue) < v) && comp2 as
                        "pomeranians" -> (fromJust (lookup s sue) > v) && comp2 as
                        "goldfish" -> (fromJust (lookup s sue) > v) && comp2 as
                        _ -> (fromJust (lookup s sue) == v) && comp2 as

getInput :: FilePath -> IO Input
getInput filepath = do
  input <- mapMaybe (match aunt) <$> lines <$> readFile filepath
  return input

part1 :: Input -> Int
part1 = fst . head . filter (\(_, aunt) -> comp aunt)

part2 :: Input -> Int
part2 = fst . head . filter (\(_, aunt) -> comp2 aunt)

main :: IO()
main = do
  input <- getInput "./input.txt"
  putStrLn $ "Part 1: " <> show (part1 input)
  putStrLn $ "Part 2: " <> show (part2 input)
