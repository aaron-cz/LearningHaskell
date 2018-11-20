-- Q1 high level description of 5 different possible representations
--    of playing cards from a standard 52 card deck.
-- A1.1: 52 different values
-- A1.2: 13 * 4 different values that could be
--       enum * Int; enum * enum; Int * Int


-- Q2 define a type for "font" tag in HTML
data Font = Font (Maybe Int) (Maybe String) (Maybe Colour)
data Colour = ColourName | ColourNumber
data ColourName = Red | Blue | Green
data ColourNumber = RGB Int Int Int

-- Q3 factorial
-- use mod y x == 0
factorial :: Int -> [Int]
factorial 0 = [0]
factorial x = factorialFrom 1 x where
    factorialFrom x y
        | x > y                   = []
        | dividable x y           = x:factorialFrom (x+1) y
        | otherwise               = factorialFrom (x+1) y
dividable :: (Ord a, Num a) => a -> a -> Bool
dividable 0 0   = False
dividable _ 0   = True
dividable x y
    | x <= 0 || x > y   = False
    | otherwise         = dividable x (y-x)


-- Q4 'elem a [a]'
myElem :: (Eq a) => a -> [a] -> Bool
myElem _ [] = False
myElem a (x:xs) = (a == x) || myElem a xs


-- Q5 longestPrefix
longestPrefix :: (Eq a) => [a] -> [a] -> [a]
longestPrefix _ [] = []
longestPrefix [] _ = []
longestPrefix (x:xs) (y:ys)
    | x == y    = x:(longestPrefix xs ys)
    | otherwise = []

-- Q6 conversion of C code
mcCarthy_91 :: Int -> Int
mcCarthy_91 n = 
    let c = 1 
    in  myLoop c n

myLoop :: Int -> Int -> Int
myLoop 0 n = n
myLoop c n = if (n > 100) then myLoop (c - 1) (n - 10)
             else myLoop (c + 1) (n + 11)

-- Q7 min to max
minToMax lower upper = [lower..upper]

min2Max :: Int -> Int -> [Int]
min2Max lower upper
    | lower > upper = []
    | otherwise     = lower:(min2Max (lower+1) upper)


     