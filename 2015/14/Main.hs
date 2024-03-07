module Main where

import Data.Char (isAlpha)
import Data.List (foldl')
import Data.Maybe (fromJust)
import Text.Regex.Applicative (RE, match, string, psym, some)
import Text.Regex.Applicative.Common (decimal)
import Data.Map.Strict qualified as M

-- /** TYPES
type Speed = Int
type Fly = Int
type Rest = Int
type Distance = Int
type Score = Int
type Time = Int
type Name = String
data Rider = Rider { sp :: Int, fl :: Int, rs :: Int , ds :: Int, sc :: Int} deriving (Show)

type Parser a = RE Char a

type Input = M.Map Name Rider


-- **/


-- /** PARSER

rider :: Parser (Name, Speed, Fly, Rest)
rider = (,,,) <$> some (psym isAlpha) <* string " can fly " <*> decimal <* string " km/s for " <*> decimal <* string " seconds, but then must rest for " <*> decimal <* string " seconds."

-- **/

mkRider :: (Name, Speed, Fly, Rest) -> (Name, Rider)
mkRider (n, s, f, r) = (n, Rider s f r 0 0)

getDistance :: Int -> Int -> Int -> Int -> Int
getDistance time s f r  =
  let d1 = (div time (f + r)) * f * s
      d2 = (min f (mod time (f + r))) * s
  in d1 + d2

isFly :: Name -> Time -> Input -> Bool
isFly name time riders =
  let rider = fromJust . M.lookup name $ riders
      (f, r) = (fl rider, rs rider)
      x = mod time (f + r)
   in x <= f && x > 0

getMaxDistance :: Input -> Distance
getMaxDistance = maximum . map (ds . snd) . M.toList

updateDistance :: Time -> Input -> Input
updateDistance t input = M.mapWithKey (\k r@(Rider s _ _ d _) -> if isFly k t input then r { ds = d + s } else r) input

updateScore :: Input -> Input
updateScore input =
  let maxDistance = getMaxDistance input
  in M.map (\r@(Rider _ _ _ d s) -> if d == maxDistance then r { sc = s + 1 } else r) input

getInput :: FilePath -> IO Input
getInput filepath = do
  input <- lines <$> readFile filepath
  return $ M.fromList.map (mkRider . fromJust . match rider) $ input

part1 :: Input -> Int
part1 = maximum . map snd . M.toList .  M.map (\(Rider s f r _ _) -> getDistance 2503 s f r)

part2 :: Input -> Int
part2 input = maximum . map (sc . snd) . M.toList . head . drop 2502 . scanl step input $ [1..]
    where step riders time = updateScore $ updateDistance time riders

main :: IO()
main = do
  riders <- getInput "./input.txt"
  putStrLn $ "Part 1: " <> show (part1 riders)
  putStrLn $ "Part 2: " <> show (part2 riders)
