module Main where

import Data.Map.Strict qualified as M
import Data.Maybe (fromJust)
import Data.List (sort, foldl')
import Data.Maybe (fromMaybe, catMaybes)
import Text.Regex.Applicative (RE, match, (=~), sym, psym, string, many, some, (<|>))
import Text.Regex.Applicative.Common (decimal)

-- /** Utilities
type Parser a = RE Char a

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p s = (:) <$> p <*> many (s *> p)

plus, minus :: forall a . (Enum a, Bounded a) => a -> Int -> a
plus v n = toEnum ((fromEnum v + n) `mod`  (fromEnum (maxBound :: a) + 1))
minus v n = plus v (-n)
inc, dec :: forall a . (Enum a, Bounded a) => a -> a
inc v = plus v 1
dec v = plus v (-1)
-- **/

type Chip = Int
data Slot = Bot Int | Output Int deriving (Show, Eq, Ord)
type Factory = M.Map Slot ([Chip], (Slot, Slot))

data Instruction = Put Slot Chip | Move Slot Slot Slot deriving Show

type Input = Factory

slot :: Parser Slot
slot = Bot <$ string "bot " <*> decimal <|> Output <$ string "output " <*> decimal

instruction = Move <$> slot <* string " gives low to " <*> slot <* string " and high to " <*> slot
          <|> flip Put  <$ string "value " <*> decimal <* string " goes to " <*> slot

loadInstruction :: Instruction -> Factory -> Factory
loadInstruction (Move s l h) = M.insert s ([], (l,h))

putChip :: Instruction -> Factory -> Factory
putChip (Put s c) = M.adjust (\(cs, i) -> (c:cs, i)) s

getFullSlots :: Factory -> [Slot]
getFullSlots = M.keys . M.filter (\(chips, _) -> length chips == 2)

emp :: Slot -> Factory -> Factory
emp s f = M.adjust (\(_, i) -> ([], i)) s f

ins :: Slot -> Chip -> Factory -> Factory
ins s c f = if M.member s f
            then M.adjust (\(cs, i) -> (c:cs, i)) s f
            else M.insert s ([c], (Bot 1, Bot 1)) f

proceed :: Slot -> Factory -> Factory
proceed s f = let slot = fromJust . M.lookup s $ f
                  (l,h) = snd slot
                  [vl, vh] = sort . fst $ slot
              in ins h vh . ins l vl . emp s $ f

runFactory :: Factory -> Factory
runFactory f = let full = getFullSlots f
               in foldr proceed f full

part1Check :: Factory -> Bool
part1Check f = let x = M.filter (\(chips, _) -> sort chips == [17,61]) f
               in length x == 1

getInput :: FilePath -> IO Input
getInput filepath = do
  input <- prepare <$> readFile filepath
  return input

prepare :: String -> Input
prepare = getFactory . catMaybes . map (match instruction) . lines

getFactory :: [Instruction] -> Factory
getFactory instructions =
  let puts = [p | p@(Put _ _) <- instructions]
      moves = [s | s@(Move _ _ _) <- instructions]
  in foldr putChip (foldr loadInstruction M.empty moves) puts


part1 :: Input -> Int
part1 = go . head . M.keys. M.filter (\(chips, _) -> sort chips == [17,61]) . head . filter part1Check .  iterate runFactory
  where go (Bot n) = n

part2 :: Input -> Int
part2 = product . map (\(_,(c, _)) -> head c) .  M.toList . M.filterWithKey (\k _ -> elem k [Output 0, Output 2, Output 1] ) . head . filter ((== 0) . length .  getFullSlots) .  iterate runFactory

main :: IO()
main = do
  input <- getInput "./input.txt"
  putStrLn $ "Part 1: " <> show (part1 input)
  putStrLn $ "Part 2: " <> show (part2 input)
