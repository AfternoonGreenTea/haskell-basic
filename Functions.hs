double :: Int -> Int
double  x = x * 2

square :: Int -> Int
square x = x * x

greet :: String -> String
greet name = "Hello, " ++ name ++ "!"

answer :: Int
answer = 42

triple :: Int -> Int
triple x = x * 3

add :: Int -> Int -> Int
add x y = x + y

add3 :: Int -> Int
add3 = add 3

multiply :: Int -> Int -> Int
multiply x y = x * y

times4 :: Int -> Int
times4 = multiply 4

abs' :: Int -> Int
abs' n = if n >= 0 then n else (-n)

gradeIf :: Int -> String
gradeIf score =
    if score >= 90 then "A"
    else if score >= 80 then "B"
    else if score >= 70 then "C"
    else if score >= 60 then "D"
    else "F"

grade :: Int -> String
grade score
  | score >= 90 = "A"
  | score >= 80 = "B"
  | score >= 70 = "C"
  | score >= 60 = "D"
  | otherwise = "F"

bmiAdvice :: Double -> Double -> String
bmiAdvice height weight
  | bmi < 18.5 = "underweight"
  | bmi < 25 = "Normal"
  | otherwise = "Overweight"
  where bmi = weight / height ^ 2

isZero :: Int -> Bool
isZero 0 = True
isZero _ = False

describeList :: [a] -> String
describeList []      = "empty"
describeList [_]     = "singleton"
describeList [_,_]   = "two elements"
describeList (_:_:_) = "longer"

dayName :: Int -> String
dayName 0 = "Sunday"
dayName 1 = "Monday"
dayName 2 = "Tuesday"
dayName 3 = "Wednesday"
dayName 4 = "Thursday"
dayName 5 = "Friday"
dayName 6 = "Saturday"
dayName _ = "Invalid day"

cylinderVolume :: Double -> Double -> Double
cylinderVolume r h =
    sideArea * h
    where
        sideArea = 2 * pi * r * h

areaOfRing :: Double -> Double -> Double
areaOfRing r1 r2 =
    let outer = pi * r1 * r1
        inner = pi * r2 * r2
    in outer - inner

clamp :: Int -> Int -> Int -> Int
clamp lo hi x
  | x < lo    = lo
  | x > hi    = hi
  | otherwise = x


fizzBuzz :: Int -> String
fizzBuzz n
  | n `mod` 15 == 0 = "FizzBuzz"
  | n `mod` 3 == 0  = "Fizz"
  | n `mod` 5 == 0  = "Buzz"
  | otherwise       = show n

maxOf3 :: Int -> Int -> Int -> Int
maxOf3 a b c = max a (max b c)
