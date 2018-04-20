-- Sections 1 & 2

len []     = 0
len (x:xs) = 1 + len xs

isEmpty [] = True
isEmpty (_:_) = False


-- Definition B 
-- iota :: Int -> [Int]

iota n = iotatr 1 n
  where
   iotatr m n 
     | m == n = [n]
     | m <  n = m : iotatr (m+1) n



-- Sections 3 & 4
data Gender = Male  | Female
data Role   = Staff | Student

data Suit = Club | Diamond | Heart | Spade
data Rank = R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | 
            J  | Q  | K  | A

data Card = Card Suit Rank


     
-- Sections 5 & 6
-- Quiz: Filtering a list
filtern :: [Int] -> [Int]
filtern [] = []
filtern (e:es)
    | e < 0     = filtern es
    | otherwise = e:filtern es
    
-- insertInt :: Int -> [Int] -> [Int]
insertInt x [] = [x]
insertInt x (e:es)
    | x < e     = (x:(e:es))
    | otherwise = (e:(insertInt x es))


    
-- f(n) = 1/2 + 1/3 + ... + 1/n
infn :: (Fractional a, Ord a) => a -> a
infn 1 = 0
infn 2 = 1/2
infn n
    | n > 2         = (1/n) + infn (n-1)
    | otherwise     = 0


























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
-- myConcat :: [t] -> [t] -> [t]
myConcat [] a = a
myConcat (x:xs) a = (x : (myConcat xs a))




-- test block --
ntom :: Int -> Int -> [Int]
ntom n m 
    | n  > m = []
    | n == m = [n]
    | n  < m = (n:ntom (n+1) m)myXOR :: Bool->Bool->Bool
myXOR a b = (a /= b)