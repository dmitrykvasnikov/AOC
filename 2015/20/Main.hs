{-# LANGUAGE FlexibleContexts #-}
module Main where
import Debug.Trace
type Input = String

getDenoms :: Int -> [Int]
getDenoms n = ds ++ dsr
  where
    lim = floor . sqrt . fromIntegral $ n
    ds = [d | d <- [1..lim], mod n d == 0]
    ds' = [div n d | d <- reverse ds]
    dsr = if (ds' !! 0) ^2 == n then tail ds' else ds'


getGifts :: Int -> Int
getGifts = (*10) . sum . getDenoms

getGifts' :: Int -> Int
getGifts' n = (*11) . sum . filter ((>=n) . (*50)) . getDenoms $ n

getInput :: FilePath -> IO Input
getInput filepath = do
  input <- readFile filepath
  return input

part1 :: Int
part1 = fst . head . dropWhile (\i -> snd i <= 33100000) . map (\i -> (i, getGifts i)) $ [1..]

part2 :: Int
part2 = fst . head . dropWhile (\i -> snd i <= 33100000) . map (\i -> (i, getGifts' i)) $ [1..]


main :: IO()
main = do
  -- putStrLn $ "Part 1: " <> show part1
  putStrLn $ "Part 2: " <> show part2
