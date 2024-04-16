module Main where

import Data.Maybe (fromMaybe, catMaybes)
import Data.Char (toLower)
import Data.List (group, isInfixOf)
import Text.Regex.Applicative (RE, match, (=~), sym, psym, string, many, some, (<|>))
import Text.Regex.Applicative.Common (decimal)
import Data.Hash.MD5 qualified as H
import Data.Map.Strict qualified as M
import Control.Monad.State.Lazy as S
import Control.Monad (guard)

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
type Input = String
type Index = Int
type Secure = Int
data Hashmap = Hashmap {inp :: String, sec :: Int, mem :: M.Map Index Hash } deriving Show

hash :: Input -> Secure -> Index -> Hash
hash inp sec ind = head . drop (sec + 1) . iterate (map toLower . H.md5s . H.Str) $ (inp <> show ind)

groups :: String -> [(Char, Int)]
groups = map ((,) <$> head <*> length) . group

check3 :: String -> Maybe Char
check3 str = let lst = dropWhile ((< 3) . snd) . groups $ str
                 in case length lst of
                    0 -> Nothing
                    _ -> Just (fst $ head lst)

check5 :: Index -> Int -> String -> State Hashmap Bool
check5 ind cnt str = do
  if (cnt <= 0)
  then return False
  else do
        h <- getHash ind
        if isInfixOf str h
        then return True
        else check5 (ind+1) (cnt-1) str

getHash :: Index -> State Hashmap Hash
getHash ind = do
  state <- get
  let h = M.lookup ind $ mem state
  case h of
    (Just val) -> return val
    Nothing -> do
      let h' = hash (inp state) (sec state) ind
      put $ state { mem = M.insert ind h' (mem state) }
      return h'

isValid:: Index -> State Hashmap Bool
isValid ind = do
  h <- getHash ind
  case check3 h of
    Nothing -> return False
    (Just c) -> check5 (ind+1) 1000 (take 5 $ repeat c)

part1, part2 :: Index -> State Hashmap [(Index, Hash)]
part1 ind = do
  h <- getHash ind
  isOk <- isValid ind
  rest <- part1 (ind + 1)
  if isOk then return $ (ind, h) : rest
          else return rest

part2 = undefined

main :: IO()
main = do
  let input = "abc"
  putStrLn $ "Part 1: " <> (show . fst . head . drop 63 . evalState (part1 0) $ (Hashmap input 0 M.empty))
  putStrLn $ "Part 2: " <> (show . fst . head . drop 63 . evalState (part1 0) $ (Hashmap input 2016 M.empty))
