module Main where

import Data.List (isPrefixOf)
type Input = [String]

parse :: String -> String
parse str = case isPrefixOf "\\x" str of
  True -> '_' : parse (drop 4 str)
  False -> case str of
            []        -> []
            ('\\':'\\':xs) -> '_' : parse xs
            ('\\':'"':xs) -> '_' : parse xs
            (x:xs)    -> x : parse xs

parseback :: String -> String
parseback str = "\"" <> go str <> "\""
  where
    go [] = []
    go ('\\':xs) = "\\\\" <> go xs
    go ('\"':xs) = "\\\"" <> go xs
    go (x:xs)    = x : go xs


getInput :: FilePath -> IO Input
getInput filepath = do
  input <- lines <$> readFile filepath
  return input

part1 :: Input -> Int
part1 = sum . map ((\x -> x - 2) . length . parse)

part2 :: Input -> Int
part2 = sum . map (length . parseback)

main :: IO()
main = do
  len <- sum . map length <$> getInput "./input.txt"
  input <- getInput "./input.txt"
  putStrLn $ "Part 1: " <> show (len - (part1 input))
  putStrLn $ "Part 2: " <> show ((part2 input) - len)
