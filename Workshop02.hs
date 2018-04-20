-- Q1 high level description of 5 different possible representatios
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
        | dividible x y           = x:factorialFrom (x+1) y
        | otherwise               = factorialFrom (x+1) y
dividible :: (Ord a, Num a) => a -> a -> Bool
dividible 0 0   = False
dividible _ 0   = True
dividible x y
    | x <= 0 || x > y   = False
    | otherwise         = dividible x (y-x)


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

-- Q6 TODO conversion between C and Haskell