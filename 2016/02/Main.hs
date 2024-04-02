module Main where

type Input = [String]

move :: Int -> Char -> Int
move 1 'U' = 1
move 2 'U' = 2
move 3 'U' = 3
move n 'U' = n - 3
move 3 'R' = 3
move 6 'R' = 6
move 9 'R' = 9
move n 'R' = n + 1
move 1 'L' = 1
move 4 'L' = 4
move 7 'L' = 7
move n 'L' = n - 1
move 7 'D' = 7
move 8 'D' = 8
move 9 'D' = 9
move n 'D' = n + 3

code :: [String]
code = [ "--1--"
       , "-234-"
       , "56789"
       , "-ABC-"
       , "--D--"
       ]

getCoord :: (Int, Int) -> Char -> (Int, Int)
getCoord (y,x) 'U'
  | y == 0 = (y,x)
  | (code !! (y-1) !! x) == '-' = (y,x)
  | otherwise = (y-1, x)
getCoord (y,x) 'D'
  | y == 4 = (y,x)
  | (code !! (y+1) !! x) == '-' = (y,x)
  | otherwise = (y+1, x)
getCoord (y,x) 'R'
  | x == 4 = (y,x)
  | (code !! y !! (x + 1)) == '-' = (y,x)
  | otherwise = (y, x + 1)
getCoord (y,x) 'L'
  | x == 0 = (y,x)
  | (code !! y !! (x - 1)) == '-' = (y,x)
  | otherwise = (y, x - 1)

getInput :: FilePath -> IO Input
getInput filepath = do
  input <- lines <$> readFile filepath
  return input

part1 :: Input -> [Int]
part1 = map (foldl move 5)

part2 :: Input -> String
part2 = map (\(y,x) -> code !! y !! x) . map (foldl getCoord (2,0))

main :: IO()
main = do
  input <- getInput "./input.txt"
  putStrLn $ "Part 1: " <> show (part1 input)
  putStrLn $ "Part 2: " <> show (part2 input)
