module Main where

import qualified AOC2024D1 as D1
import qualified AOC2024D2 as D2

getHandler :: String -> Maybe (IO (Int, Int))
getHandler "1" = Just $ D1.solution
getHandler "2" = Just $ D2.solution
getHandler _   = Nothing

main :: IO ()
main = do
  putStr "\nEnter day number (ENTER to leave): "
  day <- getLine
  case day of
    "" -> putStrLn "Bye!"
    day' -> case getHandler day' of
      Nothing  -> (putStrLn $ "No solution for day: " <> day') >> main
      Just sol -> sol >>= putStrLn . show
