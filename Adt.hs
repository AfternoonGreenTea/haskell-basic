data Color = Red | Green | Blue
  deriving (Show, Eq)

isPrimary :: Color -> Bool
isPrimary Red = True
isPrimary Green = True
isPrimary Blue = True

data Point = Point Double Double deriving (Show, Eq)
distance :: Point -> Point -> Double
distance (Point x1 y1) (Point x2 y2) = sqrt ((x1 - x2) ^ 2 + (y1 - y2) ^ 2)