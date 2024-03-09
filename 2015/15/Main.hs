module Main where

import Data.Maybe (fromJust)
import Data.List (foldl')
import Data.Char (isAlpha, isDigit)
import Data.Map.Strict qualified as M
import Text.Regex.Applicative (RE, sym, some, psym, string, match, many)
import Text.Regex.Applicative.Common (signed, decimal)

-- /** TYPES
data Props = Props { cp :: Int, du :: Int, fl :: Int, te:: Int, ca :: Int } deriving Show

type Ingridients = M.Map String Props

type Recipy = [(String, Int)]

type Input = Ingridients

type Prop = Props -> Int
-- **/

type Parser a = RE Char a

range :: [[Int]]
range = [[a,b,c,d] | a <- [0..100], b <- [0..100], c <- [0..100], d <- [0..100], a + b + c + d == 100]

parser :: Parser (String, [Int])
parser = (,) <$> some (psym (/=':')) <* string ":"  <*> some (sym ' ' *> (some (psym isAlpha)) *> sym ' ' *> (signed decimal) <* (many (sym ',')))

mkIngridient :: (String, [Int]) -> (String, Props)
mkIngridient (n, [c, d, f, t, c']) = (n, Props c d f t c')

calcProp :: Ingridients -> Recipy -> Prop -> Int
calcProp ing rec pr =
  let res = foldl' step 0 rec
  in if res < 0 then 0 else res
    where
     step res (i, v) = res + v * pr (fromJust $ M.lookup i ing)

calcProps :: Bool -> Ingridients -> Recipy -> [Prop] -> Int
calcProps flag ing rec prs =
  case flag || (calcProp ing rec ca == 500) of
    True -> foldl' step 1 prs
    False -> 0
    where step res pr = res * calcProp ing rec pr


getInput :: FilePath -> IO Input
getInput filepath = do
  input <- M.fromList . map (mkIngridient .  fromJust . match parser) <$> lines <$> readFile filepath
  return input

part1 :: Bool -> Input -> Int
part1 flag input = foldl' step 0 $ map (zip ings) range
  where
    ings = M.keys input
    step :: Int -> Recipy -> Int
    step res rec = let newRes = calcProps flag input rec [cp, fl, du, te]
                   in case newRes > res of
                    True -> newRes
                    False -> res


part2 :: Bool -> Input -> Int
part2 = undefined

main :: IO()
main = do
  input <- getInput "./input.txt"
  putStrLn $ "Part 1: " <> show (part1 True input)
  putStrLn $ "Part 2: " <> show (part1 False input)
