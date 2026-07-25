import Data.Char (toUpper)
import Text.Read (readMaybe)

isMember :: Eq a => a -> [a] -> Bool
isMember _ [] = False
isMember x (y:xs)
  | x == y = True
  | otherwise = isMember x xs

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (p:xs) =
  quickSort [x | x <- xs, x <= p]
  ++ [p]
  ++ quickSort [x | x <- xs, x > p]

maximum' :: Ord a => [a] -> a
maximum' [] = error "empty list"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

class Printable a where
  toString :: a -> String

instance Printable Bool where
  toString True  = "true"
  toString False = "false"

instance Printable Int where
  toString n = "int:" ++ show n

data Color = Red | Blue deriving (Show, Eq)

instance Printable Color where
  toString Red = "color:red"
  toString Blue = "color:blue"

printAll :: Printable a => [a] -> [String]
printAll = map toString

class Size a where
  sizeOf :: a -> Int

instance Size [a] where
  sizeOf = length

addMaybes :: Maybe Int -> Maybe Int -> Maybe Int
addMaybes mx my = (+) <$> mx <*> my

shoutMaybe :: Maybe String -> Maybe String
shoutMaybe = fmap (map toUpper)

addM :: Maybe Int -> Maybe Int -> Maybe Int
addM mx my = (+) <$> mx <*> my

parseAndDouble :: String -> Maybe Int
parseAndDouble s = do
  n <- readMaybe s
  pure (n * 2)

data Option a = None | Some a deriving (Show)

instance Functor Option where
  fmap _ None = None
  fmap f (Some x) = Some (f x)

instance Applicative Option where
  pure = Some
  None <*> _ = None
  Some f <*> mx = fmap f mx

instance Monad Option where
  None >>= _ = None
  Some x >>= f = f x
