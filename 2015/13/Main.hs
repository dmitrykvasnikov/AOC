module Main where

import Data.Char (isAlpha)
import Data.List (lookup, foldl', permutations)
import Data.Maybe (fromJust)
import Data.Map.Strict qualified as M
import Text.Regex.Applicative (RE, match, string, sym, psym, some, (<|>))
import Text.Regex.Applicative.Common (decimal)

-- /**TYPES
type Name = String
type Neighbour = String
type Input = HappyTable
type Happiness = Int
type HappyTable = M.Map Name [(Neighbour, Happiness)]
type Allocation = (Name, Happiness, Neighbour)
type Tables = [Table]
type Table = [Name]
-- **/

-- /**PARSERS
type Parser a = RE Char a

wordP :: Parser String
wordP = some (psym isAlpha)

happyP :: Parser Int
happyP = (string " would gain " *> decimal) <|> (string " would lose " *> ((negate) <$> decimal))

allocationP :: Parser Allocation
allocationP = (,,) <$> wordP <*> happyP <* string " happiness units by sitting next to " <*> wordP <* sym '.'
-- **/

makeHappyTable :: [Allocation] -> HappyTable
makeHappyTable alloc = foldl' go M.empty alloc
  where go m (nm, h, nb) = M.insertWith (\n o -> n <> o) nm [(nb, h)] m

getInput :: FilePath -> IO Input
getInput filepath = do
  content <- map (fromJust . match allocationP) <$> lines <$> readFile filepath
  return $ makeHappyTable content

isCircular :: [Name] -> [[Name]] -> Bool
isCircular n ns = go (length n) n ns where
  go 0 _ _ = False
  go x n' ns' = case elem n' ns' of
                  False -> go (x-1) (tail n' ++ [head n']) ns'
                  True -> True

removeCircular :: [[Name]] -> [[Name]]
removeCircular [] = [[]]
removeCircular (n:ns) =
 case isCircular n ns of
  True -> removeCircular ns
  False -> n : removeCircular ns

tables :: HappyTable -> Tables
tables = permutations . M.keys

happy :: Name -> Neighbour -> HappyTable -> Int
happy name neighbour   = fromJust . lookup neighbour . fromJust . M.lookup name

tableHappy :: Table -> HappyTable -> Happiness
tableHappy [] _ = 0
tableHappy (x:y:[]) m = happy x y m + happy y x m
tableHappy t m = go 0 table' where
  table' = t ++ (take 2 t)
  go s (x:y:z:rest) = go (s + happy y x m + happy y z m) (y:z:rest)
  go s _            = s

part1 :: Input -> Int
part1 input = maximum . map (flip tableHappy input) $ tables input

part2 :: Input -> Int
part2 input =  part1 input''
  where names = M.keys input
        input' = foldl' (\m n -> M.insertWith (\n o -> n <> o) n [("Dmitry", 0)] m) input names
        input'' = M.insert "Dmitry" (zip names (repeat 0)) input'


main :: IO()
main = do
  input <- getInput "./input.txt"
  putStrLn $ "Part 1: " <> show (part1 input)
  putStrLn $ "Part 2: " <> show (part2 input)
