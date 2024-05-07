module AOC where

data Coord = Coord { _x, _y :: Int } deriving (Eq, Ord, Show)

instance Semigroup Coord where
  c1 <> c2 = Coord (_x c1 + _x c2) (_y c1 + _y c2)
instance Monoid Coord where
  mempty = Coord 0 0
  mappend = (<>)

manhattan :: Coord -> Coord -> Int
manhattan c1 c2 = abs (_x c1 - _x c2) + abs (_y c1 - _y c2)
