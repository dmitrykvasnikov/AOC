module Main where

import Data.Maybe (fromMaybe, catMaybes)
import Data.Char (toLower)
import Data.List (group, isInfixOf)
import Text.Regex.Applicative (RE, match, (=~), sym, psym, string, many, some, (<|>))
import Text.Regex.Applicative.Common (decimal)
import Data.Hash.MD5 qualified as H
import Data.Map.Strict qualified as M

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

type Hash = String
type Index = Int
type Hashmap = M.Map Index Hash

groups :: String -> [(Char, Int)]
groups = map ((,) <$> head <*> length) . group

checkThree :: String -> Maybe Char
checkThree str = let lst = dropWhile ((< 3) . snd) . groups $ str
                 in case length lst of
                    0 -> Nothing
                    _ -> Just (fst $ head lst)


checkFive :: Char -> [String] -> Bool
checkFive char = any (isInfixOf (take 5 . repeat $ char))

hashes, hashes2 :: String -> [(Int, String)]
hashes input = map (fmap (map toLower . H.md5s . H.Str))
               $ zipWith (\s i -> (i, s <> show i)) (repeat input) [0..]

hashes2 input = map (fmap (head . drop 2016 . iterate (map toLower . H.md5s . H.Str)))
               $ zipWith (\s i -> (i, s <> show i)) (repeat input) [0..]
-- hashes2 = map go . hashes h
            -- where go (i, hash) = (i, hash')
                                  -- where hash' = head . drop 2015 $ iterate (map toLower . H.md5s . H.Str) hash

type Input = String

getInput :: FilePath -> IO Input
getInput filepath = do
  input <- prepare <$> readFile filepath
  return input

prepare :: String -> Input
prepare = undefined

part1 :: (String -> [(Int, String)]) -> Input -> Int
part1 h i = fst . head . drop 63 . getValid . h $ i
        where getValid [] = []
              getValid (h:hs) = case checkThree . snd $ h of
                                  Nothing -> getValid hs
                                  Just char -> case checkFive char (map snd $ take 1000 hs) of
                                    True -> h : getValid hs
                                    False -> getValid hs
part2 :: Input -> [(Int, String)]
part2 = take 10 . getValid . hashes
        where getValid (h:hs) = case checkThree . snd $ h of
                                  Nothing -> getValid hs
                                  (Just _) -> h : getValid hs

main :: IO()
main = do
  let input = "abc"
  putStrLn $ "Part 1: " <> show (part1 hashes input)
  putStrLn $ "Part 2: " <> show (part1 hashes2 input)
