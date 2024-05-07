module AOC where

type Coord = (Int, Int)

(<+>) :: Coord -> Coord -> Coord
(<+>) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

manhattan :: Coord -> Coord -> Int
manhattan (x1,y1) (x2,y2) = abs (x2 - x1) + abs (y2 - y1)
