-- defining data type
data Gender = Male  | Female
data Role   = Staff | Student

data Suit = Club | Diamond | Heart | Spade
data Rank = R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | 
            J  | Q  | K  | A

data Card = Card Suit Rank


-- myConcat :: [t] -> [t] -> [t]
myConcat [] a = a
myConcat (x:xs) a = (x : (myConcat xs a))


-- myReverse :: [a] -> [a]
myReverse []     = []
myReverse ([b])  = [b]
myReverse (x:xs) = (myReverse xs) ++ [x]


-- myXOR :: Bool->Bool->Bool
myXOR :: Bool->Bool->Bool
myXOR a b = (a /= b)