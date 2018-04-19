-- myConcat :: [t] -> [t] -> [t]
myConcat [] a = a
myConcat (x:xs) a = (x : (myConcat xs a))



-- myReverse :: [a] -> [a]
myReverse []     = []
myReverse ([b]) =  [b]
myReverse (x:xs) = (myReverse xs) ++ [x]


-- myNth :: Int -> [t] -> t
myHead []    = error "myHead called on an empty list"
myHead (x:_) = x

myNth 0 lst = (myHead lst)
myNth n []  = error "myNth called on an empty list"
myNth n (x:xs) = myNth (n-1) xs



-- test block --
ntom :: Int -> Int -> [Int]
ntom n m 
    | n  > m = []
    | n == m = [n]
    | n  < m = (n:ntom (n+1) m)
