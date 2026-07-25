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