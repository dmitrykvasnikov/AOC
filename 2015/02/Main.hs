module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Char
import Data.List

getIntFromStr :: String -> Int
getIntFromStr str = go str 0
  where
    go :: String -> Int -> Int
    go [] res = res
    go (c:cs) res = go cs (res * 10 + (digitToInt c))

getArea :: [Int] -> Int
getArea (x:y:z:_) = 3 * x * y + 2 * x * z + 2 * y * z

getSurface :: [Int] -> Int
getSurface (x:y:z:_) = 2 * (x + y) + x * y * z

input =  sum <$> map (getArea . sort) <$> map ( map (getIntFromStr . T.unpack) . (T.splitOn $ T.pack "x")) <$> T.lines <$> T.readFile "./input.txt"
main :: IO()
main =do
  sum <$> map ( getArea . sort . map (getIntFromStr . T.unpack) . (T.splitOn $ T.pack "x")) <$> T.lines <$> T.readFile "./input.txt" >>= \res -> putStrLn $ "Part 1: " <> show res
  sum <$> map ( getSurface . sort . map (getIntFromStr . T.unpack) . (T.splitOn $ T.pack "x")) <$> T.lines <$> T.readFile "./input.txt" >>= \res -> putStrLn $ "Part 2: " <> show res
