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

-- pattern matching example
matchNumber :: (Integral a) => a -> String -- String could be [Char]
matchNumber 1 = "one"
matchNumber 2 = "two"
matchNumber 3 = "three"
matchNumber x = "something else" -- if commented out, get an error for non-exhaustive pattern

-- same thing but with case
matchNumber2 :: (Integral a) => a -> String
matchNumber2 x = 
    case x of   1 -> "one"
                2 -> "two"
                3 -> "three"
                x -> "something else"

-- add vectors
addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)  
addVectors a b = (fst a + fst b, snd a + snd b) 

-- pattern matching on lists. : matches against lists with size >= 1. : can be chained
tellList :: (Show a) => [a] -> String  
tellList [] = "The list is empty"  
tellList (x:[]) = "The list has one element: " ++ show x  
tellList (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y  
tellList (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y  

-- @ is used as an alias to represent the pattern that comes after it
capital :: String -> String  
capital "" = "Empty string, whoops!"  
capital anAlias@(x:xs) = "The first letter of " ++ anAlias ++ " is " ++ [x]

-- calculate the surface of a cylinder using let bindings.
-- unlike `where` (it is only a syntactic construct), `let` is an expression
-- you could do
--  -- let a = 9 in a + 1 -- and use it instead of the value 10 in your code
cilinderSurface :: (RealFloat a) => a -> a -> a
cilinderSurface r h =
    let 
        sideSurface = r * 2 * pi * h
        baseSurface = r^2 * pi
    in sideSurface + 2 * baseSurface

-- let binding without the "in" part. Calculate bmis and display fat ones
-- let bindings cannot be used in guards
calcBmisLet :: (RealFloat a) => [(a, a)] -> [a]  
calcBmisLet xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25]  
