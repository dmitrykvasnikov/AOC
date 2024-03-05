{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}

module Parser where

import Control.Applicative hiding (some, many)
import Data.Char (isDigit, isSpace)

-- DATA/TYPE DECLARATIONS

data Input = Input { getPos :: Int
                   , getInp :: String
                   } deriving Show
data Parser a = Parser { runParser :: Input -> Either String (a, Input) }

-- TYPECLASSES INSTANCES
instance Alternative (Either String) where
  empty = Left ""
  (Left err1) <|> (Left err2) = Left (err1 <> " | " <> err2)
  (Right out) <|> _           = Right out
  _           <|> Right out   = Right out

instance Functor Parser where
  fmap f p = Parser $ \i ->
    do
      (v, rest) <- runParser p i
      return (f v, rest)

instance Applicative Parser where
  pure out  = Parser $ \inp -> Right (out, inp)
  pf <*> pv = Parser $ \inp ->
    do
      (f, rest) <- runParser pf inp
      (v, rest') <- runParser pv rest
      return (f v, rest')

instance Alternative Parser where
  empty = Parser $ const empty
  p1 <|> p2 = Parser $ \inp -> runParser p1 inp <|> runParser p2 inp

instance Monad Parser where
  return = pure
  pv >>= pf = Parser $ \inp ->
    case runParser pv inp of
      Right (out, rest) -> runParser (pf out) rest
      Left err          -> Left err

-- HELPERS

run :: Parser a -> String -> Either String (a, Input)
run parser input = (runParser parser) (Input 1 input)

pick :: Input -> Maybe (Char, Input)
pick (Input _ []) = Nothing
pick (Input pos (c:rest)) = Just (c, Input (pos+1) rest)

errmsg :: Int -> String
errmsg pos = "Error at pos: " <> show pos <> ". "

-- BASIC PARSERS

sym :: Char -> Parser Char
sym c = Parser parser where
  parser inp@(pick -> Just (c', rest))
    | c == c'   = Right (c', rest)
    | otherwise = Left $ errmsg (getPos inp) <> "Expected symbol '" <> [c]<>"', actual symbol '" <> [c'] <> "'"
  parser _      = Left $ "Unexpected end of input string when parsing char '" <>[c]<>"'"

string :: String -> Parser String
string str = Parser $ \inp ->
  case runParser (traverse sym str) inp of
    Right out -> Right out
    Left _    -> Left $ errmsg (getPos inp) <>"Expected string '" <> str <> "', actual string '" <> take (length str) (getInp inp) <> "'"

cond :: String -> (Char -> Bool) -> Parser Char
cond desc pr = Parser $ \inp ->
  case inp of
    (pick -> Just (c, rest))
      | pr c       -> Right (c, rest)
      | otherwise  -> Left $ errmsg (getPos inp) <> "Condition '" <> desc <> "' fail, actual symbol '" <> [c] <> "'"
    _              ->  Left $ "Unexpected end of input string when parsing condition '" <> desc <> "'"

psym :: (Char -> Bool) -> Parser Char
psym pr = cond "user condition" pr

many, some :: Parser a -> Parser [a]
many p = many'
  where many' = some' <|> pure []
        some' = (:) <$> p <*> many'
some p = some'
  where many' = some' <|> pure []
        some' = (:) <$> p <*> many'

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy parser sep = (:) <$> parser <*> many (sep *> parser)

sp, num :: Parser Char
sp = cond "space" isSpace
num = cond "digit" isDigit

ws, ws1 :: Parser String
ws = many sp
ws1 = some sp

decimal :: Parser Int
decimal = (read <$> some (cond "decimal" isDigit)) <|> ((*(-1)) <$> read <$> (sym '-' *> some (cond "decimal" isDigit)))

parserMain :: IO()
parserMain = putStrLn "Parser module"
