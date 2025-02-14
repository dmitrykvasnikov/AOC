module AOC2024D1 (solution, part1, part2) where

import           Control.Arrow                 ((&&&))
import           Data.Foldable                 (foldr')
import           Data.List                     (sort)
import qualified Data.Map                      as M
import           Data.Maybe                    (catMaybes, fromMaybe)
import           Text.Regex.Applicative
import           Text.Regex.Applicative.Common

type Input = ([Int], [Int])

type Parser = RE Char

line :: Parser (Int, Int)
line = (,) <$> decimal <* some (sym ' ') <*> decimal

getInput :: IO Input
getInput = do
  input <- catMaybes . map (match line) . lines <$> readFile "./input/day1"
  pure $ foldr' (\(el, er) (l, r) -> (el : l, er : r)) ([], []) input

part1 :: Input -> Int
part1 (l, r) =
  let l' = sort l
      r' = sort r
   in sum . zipWith (\x y -> abs (x - y)) l' $ r'

part2 :: Input -> Int
part2 (l, r) =
  let dict = foldl' (\m k -> M.insertWith (+) k 1 m) M.empty r
   in foldl' (\res k -> res + k * fromMaybe 0 (M.lookup k dict)) 0 l

solution :: IO (Int, Int)
solution = getInput >>= pure . (part1 &&& part2)
