module Main where

import Control.Monad.ST
import Data.Vector.Unboxed qualified as V
import Data.Vector.Unboxed.Mutable qualified as M

type Input = V.Vector Int

prepare :: String -> Input
prepare = V.fromList .  map read . lines

solve :: V.Vector Int -> (Int -> Int) -> Int
solve i f = runST $ V.thaw i >>= go 0 0
  where go c p v
          | p < 0 || p >= M.length v = return c
          | otherwise = do
                          jmp <- M.read v p
                          M.write v p $ f jmp
                          go (c+1) (p+jmp) v

main :: IO()
main = do
  input <- prepare <$> readFile "./input.txt"
  putStrLn $ "Part 1: " <> show (solve input  (+1))
  putStrLn $ "Part 2: " <> show (solve input (\j -> if j >= 3 then j - 1 else j + 1))
