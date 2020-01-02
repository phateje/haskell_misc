-- implement custom version of sum for an array of numbers
sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum'(xs)

-- implements custom version of function that returns the tail of an array
tail' :: [a] -> [a]
tail' (_:xs) = xs

-- calculates bmi given weigth and height
bmitell :: (Num a, Ord a, Fractional a) => a -> a -> [Char]
bmitell weight height 
    | bmi <= skinny = "skinny"
    | bmi <= normal = "normal"
    | bmi <= fat = "fat"
    | otherwise = "obese"
    where bmi = weight / height ^ 2
          skinny = 18.5
          normal = 25
          fat = 30

-- another way to do bmis
calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi (w, h) | (w, h) <- xs]
    where bmi (weight, height) = weight / height ^ 2