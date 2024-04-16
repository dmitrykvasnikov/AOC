module Main where

import Data.ByteString.Char8 (pack)
import Data.Maybe (fromMaybe, catMaybes)
import Data.Char (toLower)
import Data.List (group, isInfixOf, sort, scanl')
import Data.Map.Strict qualified as M
import Crypto.Hash
import Control.Monad.State.Lazy as S

hash'' bs = show ((hash . pack $ bs) :: Digest MD5)

hash' :: Input -> Secure -> Index -> Hash
hash' inp sec ind = head . drop (sec + 1) . iterate (map toLower . hash'') $ (inp <> show ind)

type Hash = String
type Input = String
type Index = Int
type Secure = Int
data Hashmap = Hashmap {inp :: String, sec :: Int, mem :: M.Map Index Hash } deriving Show


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
      let h' = hash' (inp state) (sec state) ind
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
  let input = "ngcjuoqr"
  putStrLn $ "Part 1: " <> (show . fst . head . drop 63 . evalState (part1 0) $ (Hashmap input 0 M.empty))
  putStrLn $ "Part 2: " <> (show . fst . head . drop 63 . evalState (part1 0) $ (Hashmap input 2016 M.empty))
