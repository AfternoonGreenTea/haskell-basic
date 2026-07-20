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