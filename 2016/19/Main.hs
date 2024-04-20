module Main where

import Debug.Trace
import Data.Sequence qualified as S

type Input = Int

dropElf :: [Int] -> [Int]
dropElf (x:_:rest) = x : dropElf rest
dropElf x          = x

playElf :: [Int] -> [Int]
playElf inp
  | l == 1 = inp
  | even l = res
  | otherwise = let l' = length res in drop (l' - 1) res ++ take (l' - 1) res
  where
    l = length inp
    res = dropElf inp

playElf2 :: S.Seq Int -> S.Seq Int
playElf2 elfs = prefix S.>< (suffix S.|> first)
  where target = S.length elfs `div` 2
        first = S.index elfs 0
        prefix = S.drop 1 $ S.take target elfs
        suffix = S.drop (target + 1) elfs


part1 :: Input -> Int
part1 n = head . head . filter ((== 1) . length) . iterate playElf $ [1..n]

part2 :: Input -> Int
part2 n = flip S.index 0 . head . filter ((== 1) . S.length)  . iterate playElf2 $ S.fromList [1..n]

main :: IO()
main = do
  let input = 3017957
  putStrLn $ "Part 1: " <> show (part1 input)
  putStrLn $ "Part 2: " <> show (part2 input)
