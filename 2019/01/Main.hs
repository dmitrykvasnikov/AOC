module Main where

import Data.Char

input = lines <$> readFile "./input.txt"

getValue :: String -> Int
getValue str =
  let
    v1 = digitToInt . head  $ dropWhile isLetter str
    v2 = digitToInt . head  $ dropWhile isLetter $ reverse str
  in v1*10 + v2


main :: IO()
main = (sum . map getValue) <$> input >>= putStrLn . show

