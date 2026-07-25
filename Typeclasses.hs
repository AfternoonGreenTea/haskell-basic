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
