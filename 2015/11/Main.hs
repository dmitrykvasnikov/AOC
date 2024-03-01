module Main where

import Data.Char
import Data.List
import Data.Set qualified as S

type Input = String
type Pos = Int

-- conditions for password
check1 :: Input -> Bool
check1 inp
  | length inp >= 3 = let (x:y:z:_) = inp
                  in if ord y == ord x + 1 && ord z == ord y + 1 then True else check1 $ tail inp
  | otherwise = False

check2 :: Input -> Bool
check2 = all (not . flip elem ['i', 'o', 'l'])

check3 :: Input -> Bool
check3 str = (length . filter (\s -> length s >= 2) . S.toList . S.fromList . group $ str) >= 2

shiftLetter :: Char -> Char
shiftLetter 'z' = 'a'
shiftLetter c = chr (ord c + 1)

shiftPos :: Pos -> Int -> Pos
shiftPos pos len = if pos == 0 then (len - 2) else pos - 1

step :: (Input, Pos) -> (Input, Pos)
step (pass, pos) =
  let
    c = shiftLetter $ pass !! pos
    pass' = take pos pass <> [c] <> drop (pos + 1) pass
  in
   case c of
    'a' -> case pos of
            0 -> (pass', length pass' - 1)
            _ -> step (pass', pos - 1)
    _   -> (pass', length pass' - 1)

part1 :: Input -> Input
part1 input = fst . head . dropWhile (\(pass, _) -> not $ check1 pass && check2 pass && check3 pass) $ iterate step (input, length input - 1)


part2 :: Input -> Input
part2 input = fst . head . dropWhile (\(pass, _) -> not $ check1 pass && check2 pass && check3 pass) $ iterate step (input, length input - 1)

main :: IO()
main = do
  let input1 = "cqjxjnds"
  let input2 = "cqjxxyzz"
  putStrLn $ "Part 1: " <> (part1 input1)
  putStrLn $ "Part 2: " <> (part2 $ fst . step $ (input2, length input2 - 1))
