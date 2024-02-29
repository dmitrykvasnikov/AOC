module Main where

import Data.Map qualified as M
import Data.Char (isAlpha)
import Data.List (foldl', sortOn, sort)
import Data.Maybe (fromJust)
import Text.Regex.Applicative (RE, some, psym, match, string)
import Text.Regex.Applicative.Common (decimal)

type Input = Map

type City = String
type Distance = Int
type Routes = [(City, Distance)]
type Map = M.Map City Routes

-- Parsing
type Parser a = RE Char a

spaces :: Parser String
spaces = some $ psym (==' ')

stringP :: Parser String
stringP = some $ psym isAlpha

routeP :: Parser (City, City, Distance)
routeP = (,,) <$> stringP <* spaces <* string "to" <* spaces <*> stringP <* spaces <* string "=" <* spaces <*> decimal

-- Creating Map from input
addRoute :: City -> City -> Distance -> Map -> Map
addRoute c1 c2 dist map =
  case M.member c1 map of
    False -> M.insert c1 [(c2, dist)] map
    True  -> M.adjust (\routes -> (c2, dist) : routes) c1 map

getInput :: FilePath -> IO Map
getInput filepath = do
  input <- map (fromJust . match routeP) <$> lines <$> readFile filepath
  return $ foldl' (\m (c1, c2, d) -> addRoute c2 c1 d $ addRoute c1 c2 d m) M.empty input


intersect :: [City] -> Routes -> Routes
intersect visited neighours = filter (\(c, _) -> not $ elem c visited) neighours

getDistance :: City -> [City] -> Int -> Map -> ([(Int, [City])] -> [(Int, [City])]) -> (Int, [City])
getDistance city visited dst m f =
  let toVisit = intersect visited $ fromJust $ M.lookup city m
  in case toVisit of
    [] -> (dst, city:visited)
    otherwise -> head . f . sortOn fst $ map (\(c,d) ->
      getDistance c (city : visited) (dst + d) m f)
      toVisit

part1 :: Input -> (Int, [City])
part1 input = head . sortOn fst $ map (\c -> getDistance c [] 0 input id) $ M.keys input

part2 :: Input -> (Int, [City])
part2 input = head . reverse . sortOn fst $ map (\c -> getDistance c [] 0 input reverse) $ M.keys input

main :: IO()
main = do
  input <- getInput "./input.txt"
  putStrLn $ "Part 1: " <> show (part1 input)
  putStrLn $ "Part 2: " <> show (part2 input)
