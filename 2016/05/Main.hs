module Main where

import Data.List (isPrefixOf, elem)
import Data.Char (digitToInt)
import Data.Hash.MD5 qualified as H

hash :: String -> String -> [String]
hash input prefix = filter (isPrefixOf prefix) .
                    map (H.md5s . H.Str)
                    $ zipWith (<>) (repeat input) (map show ([1..]))

part1 :: String -> String -> String
part1 i p =  map (head . drop 5) . take 8 . hash i $ p

part2 :: String -> String -> String
part2 i p = go "........" $ hash i p
  where go pass (h:hs)
          | not $ elem '.' pass = pass
          | h !! 5 < '0' || h !! 5 > '7' = go pass hs
          | otherwise = let ind = digitToInt (h !! 5) in
                        case pass !! ind of
                          '.' -> go (take ind pass ++ [h !! 6] ++ drop (ind + 1) pass) hs
                          _ -> go pass hs

main :: IO()
main = do
  putStrLn $ "Part 1: " <> show (part1 "uqwqemis" "00000")
  putStrLn $ "Part 2: " <> show (part2 "uqwqemis" "00000")
