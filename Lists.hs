nums :: [Int]
nums = [1, 2, 3, 4, 5]

chars :: [Char]
chars = ['a', 'b', 'c']

head' :: [a] -> a
head' (x:_) = x

tail' :: [a] -> [a]
tail' (_:xs) = xs

third :: [a] -> a
third (_:_:x:_) = x

sumList :: [Int] -> Int
sumList [] = 0
sumList (x:xs) = x + sumList xs

length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs

product' :: [Int] -> Int
product' [] = 1
product' (x:xs) = x * product' xs

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' y (x:xs)
  | y == x = True
  | otherwise = elem' y xs

doubleAll :: [Int] -> [Int]
doubleAll xs = map (* 2) xs

evens :: [Int] -> [Int]
evens xs = filter even xs

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
  | p x = x : filter' p xs
  | otherwise = filter' p xs

