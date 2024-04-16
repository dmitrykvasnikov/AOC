module Main where

import Data.Maybe (fromMaybe, catMaybes)
import Data.Char (toLower)
import Data.List (group, isInfixOf, sort, scanl')
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
type Input = String
type Index = Int
type Secure = Int
type Candidate = (Index, Hash)
type Candidates = M.Map Index String
type Result = [Index]
type State = (Candidates, Result)

hash :: Input -> Secure -> Index -> Hash
hash inp sec ind = head . drop (sec + 1) . iterate (map toLower . H.md5s . H.Str) $ (inp <> show ind)

groups :: String -> [(Char, Int)]
groups = map ((,) <$> head <*> length) . group

check3 :: String -> Maybe String
check3 [] = Nothing
check3 (x:[]) = Nothing
check3 (x:y:[]) = Nothing
check3 i@(x:y:z:_) = if (x == y) && (x == z)
                      then (Just (take 5 . repeat $ x))
                      else check3 $ tail i

isCandidate :: State -> Candidate -> State
isCandidate s@(c, r) (i, h) = case check3 h of
  Nothing -> s
  Just str -> (M.insert i str c, r)

updateResult :: State -> Candidate -> State
updateResult s@(c, r) (i, h) =
  let toCheck = M.toList $ M.filterWithKey (\k _ -> k + 1000 > i && k /= i) c
  in foldl update s toCheck
    where update (c', r') (i', str) = case isInfixOf str h of
                                        False -> (c', r')
                                        True -> (M.delete i' c', sort $ r' ++ [i'])

part1 :: Secure -> Input -> [Int]
part1 sec inp = take 64 . head . filter ((>64) . length) . map snd . scanl' go (M.empty, []) $ map ((,) <$> id <*> hash inp sec) [0..]

    where go :: State -> (Index, Hash) -> State
          go s c = (updateResult (isCandidate s c) c)

main :: IO()
main = do
  let input = "abc"
  putStrLn "Done"
