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

rename :: String -> User -> User
rename newName user = user { userName = newName }

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

safeDiv :: Double -> Double -> Maybe Double
safeDiv _ 0 = Nothing
safeDiv x y = Just(x / y)

describeHead :: Show a => [a] -> String
describeHead xs =
  case safeHead xs of
    Nothing -> "Empty list"
    Just x -> "head is" ++ show x

fromMaybe' :: a -> Maybe a -> a
fromMaybe' def Nothing = def
fromMaybe' _ (Just x) = x

safeIndex :: Int -> [a] -> Maybe a
safeIndex n xs
  | n < 0 = Nothing
  | otherwise = go n xs
  where
    go _ [] = Nothing
    go 0 (x:_) = Just x
    go i (_:ys) = go (i - 1) ys

data DivError = DivisionByZero | NegativeInput deriving (Show, Eq)

safeDivE :: Double -> Double -> Either DivError Double
safeDivE _ 0 = Left DivisionByZero
safeDivE x y
  | x < 0 || y < 0 = Left NegativeInput
  | otherwise = Right (x / y)
