import Data.List
import Data.Char
import qualified Data.Map as Map

jelem :: Eq t => t -> [t] -> Bool
jelem _ [] = False
jelem a (x:xs) = if a == x then True else jelem a xs

-- with list comprehensions! jelem n xs = length [a | a <- xs, a == n] > 0

jnub [] ret = ret
jnub (x:xs) rs = foo
    where foo = if x `jelem` rs then jnub xs rs else jnub xs (rs ++ [x]) -- ++ is badly inefficient on linked lists, check out the GHC implementation: https://downloads.haskell.org/~ghc/6.12.1/docs/html/libraries/base-4.2.0.0/src/Data-List.html#nub


{-
step by step execution of nub' [1,2,2,3] [] is

1 : nub' [2,2,3] [1]
    2 : nub' [2,3] [2,1]
        : nub' [3] [2,1]
          3 : nub' [] [3,2,1]
              : []
-}
jjnub l                 = nub' l []           
  where
    nub' [] _           = []                    
    nub' (x:xs) ls                              
        | x `elem` ls   = nub' xs ls            
        | otherwise     = x : nub' xs (x:ls)    


-- fun implementation of the method isInfixOf which returns true if the substring we're looking for (needle) appears in the string (haystack)
  -- tails haystack -> returns all the tails of the list.. [1,2,3,4], then [2,3,4] then [3,4] etc
  -- take nlen x -> takes the length of the needle, and see if the first chars of the haystack match it.
search :: (Eq a) => [a] -> [a] -> Bool  
search needle haystack =   
    let nlen = length needle  
    in  foldl (\acc x -> if take nlen x == needle then True else acc) False (tails haystack)

caesarEncode :: Int -> String -> String
caesarEncode offset str =
  let 
    ords = map ord str
    shifted = map ( + offset ) ords 
  in map chr shifted

-- and now with composition!
caesarEncode' :: Int -> String -> String
caesarEncode' offset str = map (chr . ( + offset) . ord) str

-- unsafe implementation, head of [] errors out
findKeyUnsafe :: (Eq k) => [(k , v)] -> k -> v
findKeyUnsafe xs key = snd . head . filter(\(k, v) -> k == key) $ xs

findKeySafe :: (Eq k) => k -> [(k, v)] -> Maybe v
findKeySafe key = foldr(\(k, v) acc -> if key == k then Just v else acc) Nothing

-- unsafe implementation, head of [] errors out
fsu :: (Eq k) => [(k , v)] -> k -> [(k , v)]
fsu xs key = filter(\(k, v) -> k == key) $ xs

{------------------------
-- usage of Either type
------------------------
given:
  - map of locker # -> locker

we want a method that can set the desired code on a locker. We need to handle both 
the case for an invalid locker number (does not exist) and
the case for when the locker is already taken, so the request can't be fulfilled
-}

-- the state for the given locker
data LockerState = Taken | Free deriving (Show, Eq)

-- definition of the map of Int -> (Bool, String)
type LockerCode = String
type LockerMap = Map.Map Integer (LockerState, LockerCode)

-- remember, Left of either is usually the exception / error, Right is usually the desired value
lockerLookup :: Integer -> LockerMap -> Either String LockerCode  
lockerLookup lockerNumber map =   
  case Map.lookup lockerNumber map of   
      Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"  
      Just (state, code) -> if state /= Taken   
                              then Right code  
                              else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"

lockerLookup2 :: Integer -> LockerMap -> Either String LockerCode  
lockerLookup2 lockerNumber map =
  case Map.lookup lockerNumber map of 
    Nothing               -> Left $ "Locker " ++ show lockerNumber ++ " does not exist"
    Just (st, cd) 
      | st == Taken       -> Left $ "Locker " ++ show lockerNumber ++ " is already taken"
      | otherwise         -> Right cd

lockerLookup3 :: Integer -> LockerMap -> Either String LockerCode  
lockerLookup3 lockerNumber map =
  case Map.lookup lockerNumber map of 
    Nothing               -> Left $ "Locker " ++ show lockerNumber ++ " does not exist"
    Just (Taken, _)       -> Left $ "Locker " ++ show lockerNumber ++ " is already taken"
    Just (_, cd)          -> Right cd

-- some data, use like so: lockerLookup 100 lockers
lockers = Map.fromList   
    [(100,(Taken,"ZD39I"))  
    ,(101,(Free,"JAH3I"))  
    ,(103,(Free,"IQSA9"))  
    ,(105,(Free,"QOTSA"))  
    ,(109,(Taken,"893JJ"))  
    ,(110,(Taken,"99292"))  
    ]  