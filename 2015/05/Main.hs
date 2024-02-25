module Main where
import Data.Text as T (length, filter, tail, Text, pack, unpack, splitAt, isInfixOf, lines)
import Data.Text.IO as T (readFile)
import Data.Set as S

combinations :: [Text]
combinations = [T.pack x | x <- ["ab", "cd", "pq", "xy"]]

checkVowels :: Text-> Bool
checkVowels = (>2) . T.length . T.filter (flip Prelude.elem ("aeiou"))

checkRepeat :: String -> Bool
checkRepeat [] = False
checkRepeat (x:[]) = False
checkRepeat (x:y:rest) = case x == y of
                            True -> True
                            False -> checkRepeat (y:rest)

checkCombinations :: Text -> Bool
checkCombinations text = Prelude.all (\cmb -> not $ isInfixOf cmb text) combinations

checkRepeat2_1 :: Text -> Bool
checkRepeat2_1 text
  | T.length text < 4 = False
  | otherwise = let (f,s) = T.splitAt 2 text
                in case T.isInfixOf f s of
                  True -> True
                  False -> checkRepeat2_1 $ T.tail text

checkRepeat2_2 :: String -> Bool
checkRepeat2_2 [] = False
checkRepeat2_2 (_:[]) = False
checkRepeat2_2 (_:_:[]) = False
checkRepeat2_2 (x:y:z:rest) = case x == z of
  True -> True
  False -> checkRepeat2_2 (y:z:rest)


part1 :: FilePath -> IO Int
part1 filepath = do
  input <- T.lines <$>  T.readFile filepath
  let result = Prelude.length $ Prelude.filter (\text -> (checkVowels text) && (checkRepeat $ T.unpack text) && (checkCombinations text)) input
  return result

part2 :: FilePath -> IO Int
part2 filepath = do
  input <- T.lines <$>  T.readFile filepath
  let result = Prelude.length $ Prelude.filter (\text -> (checkRepeat2_1 text) && (checkRepeat2_2 $ T.unpack text)) input
  return result

main :: IO()
main = do
  part1 "./input.txt" >>= \res -> putStrLn $ "Part 1: " <> show res
  part2 "./input.txt" >>= \res -> putStrLn $ "Part 2: " <> show res
