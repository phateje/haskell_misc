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

-- returns the maximum from a list
getMaximum :: (Ord a) => [a] -> a
getMaximum [] = error "empty list"
getMaximum [x] = x
getMaximum (x:xs)
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = getMaximum xs 

{-
replicate takes an Int and some element and returns a list that has several repetitions of the same element. For instance, replicate 3 5 returns [5,5,5]
replicate x 0 times -> []
replicate x 1 times -> [x]
replicate x 2 times -> x : replicate x 1
-}
replicate' :: (Ord a, Num a) => a -> x -> [x]
replicate' 0 x = error "can't replicate something 0 times"
replicate' 1 x = [x]
replicate' q x = x : replicate' (q - 1) x

-- textbook solution
replicate'' :: (Num i, Ord i) => i -> a -> [a]  
replicate'' n x  
    | n <= 0    = []  
    | otherwise = x:replicate' (n-1) x  

-- take num list[] -> takes first num elements from list
take' :: (Num n, Ord n) => n -> [x] -> [x]
take' n _    
    | n <= 0    = []
take' _ []      = [] 
take' n (x:xs)  = x : take' (n - 1) xs

-- reverse a list
reverse' :: [x] -> [x]
reverse' []     = []
reverse' (x:xs) = reverse' xs ++ [x]

{-
zip takes two lists and zips them together. zip [1,2,3] [2,3] returns [(1,2),(2,3)], 
because it truncates the longer list to match the length of the shorter one. 
How about if we zip something with an empty list? Well, we get an empty list back then. So there's our edge condition. 
However, zip takes two lists as parameters, so there are actually two edge conditions.
-}
zip' :: [x] -> [y] -> [(x,y)]
zip' [] _   = []
zip' _ []   = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

-- omg, so elegance
quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' [x] = [x] -- not necessary, but should prevent unneccessary calls to sort a list of 1
quicksort' (x:xs) = 
    let 
        smaller = [a | a <- xs, a < x]
        bigger = [a | a <- xs, a >= x]
    in quicksort' smaller ++ [x] ++ quicksort' bigger

-- pretty cool implementations of merge sort: https://stackoverflow.com/questions/37082925/haskell-merge-sort
 
-- produce collatz chain
collatzChain :: (Integral a) => a -> [a]
collatzChain 1  = [1]
collatzChain x
    | odd x     = x : collatzChain (x * 3 + 1)
    | otherwise = x : collatzChain (div x 2)

-- [head ret | ret <- map collatzChain [1..100], length ret > 15] to find a list of all numbers whose resulting chain is > 15

