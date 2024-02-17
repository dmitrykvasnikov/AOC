module Main where

flr :: String -> Int
flr [] = 0
flr (x:xs) = case x of
                '(' -> 1 + flr xs
                ')' -> (-1) + flr xs
                '\n' -> 0

basement:: String -> Int
basement str = length . takeWhile (/= (-1)) $ scanl (\f b -> if b == '(' then f + 1 else f - 1) 0 str

main :: IO()
main = do
  input <- readFile "./input.txt"
  let floor_res = flr input
  let basement_res = basement input
  putStrLn $ "Floor: " ++ show floor_res
  putStrLn $ "Basement :" ++ show basement_res
  putStrLn "Done"
