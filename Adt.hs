data Color = Red | Green | Blue
  deriving (Show, Eq)

isPrimary :: Color -> Bool
isPrimary Red = True
isPrimary Green = True
isPrimary Blue = True

data Point = Point Double Double deriving (Show, Eq)
distance :: Point -> Point -> Double
distance (Point x1 y1) (Point x2 y2) = sqrt ((x1 - x2) ^ 2 + (y1 - y2) ^ 2)

data Shape = Circle Double | Rect Double Double deriving (Show, Eq)
area :: Shape -> Double
area (Circle r) = pi * r * r
area (Rect w h) = w * h

data User = User
  { userId :: Int
  , userName :: String
  , userEmail ::String
  } deriving (Show, Eq)

alice :: User
alice = User
  { userId = 1
  , userName = "alice"
  , userEmail = "alice@example.com"
  }
