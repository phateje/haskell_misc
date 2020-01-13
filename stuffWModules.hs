import Data.List

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