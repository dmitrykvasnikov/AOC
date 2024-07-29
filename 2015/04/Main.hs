module Main where

import qualified Data.Hash.MD5 as H
import           Data.List     (isPrefixOf)

type Input = String

getInput :: FilePath -> IO Input
getInput filepath = do
  input <- readFile filepath
  return input

hash :: String -> String -> String
hash input prefix = snd . head . dropWhile (not . isPrefixOf prefix. fst) .
                    map ((,) <$> H.md5s . H.Str <*> id)
                    $ zipWith (<>) (repeat input) (map show ([1..]))

part1 :: Input -> String -> String
part1 = hash

part2 :: Input -> String -> String
part2 = hash

main :: IO()
main = do
  let input = "ckczppom"
  putStrLn $ "Part 1: " <> show (part1 input "00000")
  putStrLn $ "Part 2: " <> show (part2 input "000000")
