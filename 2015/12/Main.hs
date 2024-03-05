module Main where

import Control.Applicative hiding (some, many)
import Parser qualified as P
import Data.Char (isAlphaNum)
import Data.Either (fromRight)

type Key = String
data JValue = JNum Int
            | JString String
            | JArray [JValue]
            | JObject [(Key, JValue)]
            deriving (Eq, Ord, Show)

type Input = JValue

key :: P.Parser String
key = P.ws *> P.sym '\"' *> P.some (P.cond "object key" isAlphaNum) <* P.sym '\"' <* P.ws

jnum :: P.Parser JValue
jnum = JNum <$ P.ws <*> P.decimal <* P.ws

jstring :: P.Parser JValue
jstring = JString <$ P.ws <* P.sym '\"' <*> P.many (P.psym (/='\"')) <* P.sym '\"' <* P.ws

jarray :: P.Parser JValue
jarray = JArray <$ P.ws <* P.sym '[' <*> (P.sepBy jvalue (P.sym ',')) <* P.sym ']' <* P.ws

jobject :: P.Parser JValue
jobject = JObject <$ P.ws <* P.sym '{' <*> (P.sepBy tuple (P.sym ',')) <* P.sym '}' <* P.ws
  where
    tuple = (,) <$ P.ws <*> key <* P.ws <* P.sym ':' <* P.ws <*> jvalue

jvalue :: P.Parser JValue
jvalue = jnum <|> jstring <|> jarray <|> jobject

getInput :: FilePath -> IO Input
getInput filepath = do
  input <- (fst . fromRight (JNum 0, P.Input 0 "")) <$> (P.run jvalue) <$> readFile filepath
  return input

jsum1 :: JValue -> Int
jsum1 (JNum n) = n
jsum1 (JString _) = 0
jsum1 (JArray arr) = sum . map jsum1 $ arr
jsum1 (JObject obj) = sum . map (jsum1 . snd) $ obj

jsum2 :: JValue -> Int
jsum2 (JNum n) = n
jsum2 (JString _) = 0
jsum2 (JArray arr) = sum . map jsum2 $ arr
jsum2 (JObject obj) = case (length . filter ((==(JString "red")) . snd)) obj of
  0 -> sum . map (jsum2 . snd) $ obj
  _  -> 0

part1 :: Input -> Int
part1 = jsum1

part2 :: Input -> Int
part2 = jsum2

main :: IO()
main = do
  input <- getInput "./input.txt"
  putStrLn $ "Part 1: " <> show (part1 input)
  putStrLn $ "Part 2: " <> show (part2 input)
