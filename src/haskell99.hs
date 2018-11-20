module haskell99 where

import Data.List
-- 7 Problem 7
-- (**) Flatten a nested list structure.
data NestedList a = Elem a | List [NestedList a]
flatten :: (Show a) => (NestedList a) -> [a]
flatten (Elem v)   = [v]
flatten (List xs)  = foldl (++) [] (map flatten xs)

-- 8 Problem 8
-- (**) Eliminate consecutive duplicates of list elements.
compress :: (Eq a) => [a] -> [a]
compress xs = map head (group xs)


-- 9 Problem 9 (group)
-- (**) Pack consecutive duplicates of list elements into sublists.
pack :: (Eq a) => [a] -> [[a]]
pack xs = foldl packl [] xs

packl :: (Eq a) => [[a]] -> a -> [[a]]
packl [] x = [[x]]
packl ls x
    | elem x (last ls) = init ls ++ [(last ls) ++ [x]]
    | otherwise        = ls ++ [[x]]

pack' xs = foldr packr [] xs
packr :: (Eq a) => a -> [[a]] -> [[a]]
packr x [] = [[x]]
packr x ls
    | elem x (head ls) = [[x] ++ (head ls)] ++ (tail ls)
    | otherwise        = [[x]] ++ ls

-- 10 Problem 10
-- (*) Run-length encoding of a list.
encode :: (Eq a) => [a] -> [(Int, a)]
encode xs = map summary (pack' xs)
    where summary :: (Eq a) => [a] -> (Int, a)
          summary x = (length x, head x)


-- 11 Problem 11
-- modify encode in 10 so that single elements in xs are encoded as is
data EncodedListNode a = Singleton a | Duplicates Int a
     deriving Show
encodeModified :: (Eq a) => [a] -> [EncodedListNode a]
encodeModified xs = map encodeNode (pack' xs)
    where encodeNode :: (Eq a) => [a] -> EncodedListNode a
          encodeNode [x]    = Singleton x
          encodeNode (x:xs) = Duplicates (length xs + 1) x

-- list comprehension solution
-- encodeModified xs = [y | x <- group xs, 
--                          let y = if (length x) == 1 then Single (head x)
--                                  else Multiple (length x) (head x)]


-- Problem 12
-- decode encoded list
decodeListNode :: EncodedListNode a -> [a]
decodeListNode (Singleton    x)   = [x]
decodeListNode (Duplicates n x)   = replicate n x

decodeList :: [EncodedListNode a] -> [a]
decodeList xs = concatMap decodeListNode xs


{- # INLINE decode 
decode :: Eq a => [(Int,a)] -> [a]
decode xs = build (\c n ->
  let
    f (1, x) r = x `c` r
    f (k, x) r = x `c` f (k-1, x) r
  in
    foldr f n xs)
#-}

-- Problem 13
-- (**) Run-length encoding of a list (direct solution). 
-- I.e. don't explicitly create the sublists containing the duplicates.
encode13 :: (Eq a) => [a] -> [EncodedListNode a]
encode13 = foldl encodeNodeFold []
encodeNodeFold :: (Eq a) => [EncodedListNode a] -> a -> [EncodedListNode a]
encodeNodeFold [] x  = [Singleton x]
encodeNodeFold xs x  = case (last xs) of
    (Singleton y)    ->  ift (y == x)  (ixs ++ [Duplicates 2 y]) (xs ++ [Singleton x])
    (Duplicates n y) ->  ift (y == x)  (ixs ++ [Duplicates (n+1) y]) (xs ++ [Singleton x])
    where ixs = init xs



ift :: Bool -> a -> a -> a
ift True  x _ = x
ift False _ y = y


-- Problem 14
duplicate14 :: [a] -> [a]
-- @Deprecated
-- (concatMap  (uncurry replicate)) . (zip $ repeat 2)
duplicate14 = concatMap (replicate 2) 


-- Problem 15
duplicate15 :: [a] -> Int -> [a]
duplicate15 xs n = concatMap (replicate n) xs


-- Problem 16
-- (**) Drop every N'th element from a list.
dropNth :: [a] -> Int -> [a]
dropNth xs n
    | n > (length xs) = xs
    | otherwise       = (init $ take n xs) ++ (dropNth (drop n xs) n)

-- One Suggested Solution:
-- dropEvery xs n = map fst $ filter ((n/=) . snd) $ zip xs (cycle [1..n])
-- [Lesson] filter | map approaches do not suit this problem, unless like above.


-- Problem 17
-- (*) Split a list into two parts; the length of the first part is given.
-- Do not use any predefined predicates. 
split17 :: [a] -> Int -> ([a], [a])
split17 [] _     = ([], [])
split17 xs 0     = ([], xs)
split17 (x:xs) n = ([x] ++ part1, part2) where
                   (part1, part2) = split17 xs (n - 1)


-- Problem 18
slice18 :: [a] -> Int -> Int -> [a]
slice18 xs i j = drop (i - 1) $ take j xs


-- Problem 19
-- (**) Rotate a list N places to the left.
rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate xs n = (snd xs') ++ (fst xs')
    where n'  = mod ((mod n l) + l) l
          l   = length xs
          xs' = splitAt n' xs

-- Problem 20
-- (*) Remove the K'th element from a list.
removeAt :: [a] -> Int -> [a]
removeAt xs n = map fst $ filter ((/=n).snd) (zip xs [1..])

-- Problem 21
-- Insert an element at a given position into a list. 
insertAt :: Int -> a -> [a] -> [a]
insertAt 0 x xs = x:xs
insertAt n x xs = fst xs' ++ (x:snd xs')
    where xs' = splitAt (n-1) xs

-- Problem 22
-- Create a list containing all integers within a given range.
-- range x y = [x..y]
--  or
-- range = enumFromTo
--  or
-- range x y = take (y-x+1) $ iterate (+1) x

-- Problem 23 - 25 
-- import System.Random
-- import Control.Monad

-- Problem 26 combinations
combinations :: Int -> [a] -> [[a]]
-- special case
combinations 0 _  = [[]]
-- regular case
combinations _ [] = []
combinations 1 xs = map (\x -> [x]) xs
combinations n xs = [(y:ys) | (y:yys) <- tails xs,
                               ys <- combinations (n-1) yys]

-- Problem 27
-- Group the elements of a set into disjoint subsets. 
-- * (group '(aldo beat carla david evi flip gary hugo ida) '(2 2 5))
-- ( ( (ALDO BEAT) (CARLA DAVID) (EVI FLIP GARY HUGO IDA) )
-- ... )
groups :: Eq a => [a] -> [Int] -> [[[a]]]
groups xs [] = [[]]
groups xs columns
    | length xs == sum columns =
          [ y:ys | y  <- combinations (head columns) xs,
                   ys <- groups (xs \\ y) (tail columns)]
    | otherwise = error "illegal parameters"


-- Problem 28
-- (a) sort by length
-- (b) sort by length frequency
sortByLength :: [[a]] -> [[a]]
sortByLength = sortBy (\l1 l2 -> compare (length l1) (length l2))

groupByLength :: [[a]] -> [[[a]]]
groupByLength = foldr merge [] where
    merge :: [a] -> [[[a]]] -> [[[a]]]
    merge x []     = [[x]]
    merge x (g:gs) = if (length x == length (head g))
                     then (x:g):gs
                     else [x]:g:gs

sortByLengthFreq :: [[a]] -> [[a]]
sortByLengthFreq = concat . sortByLength . groupByLength . sortByLength





-- ### Arithmatic
-- Problem 31
isPrime :: Integer -> Bool
isPrime = flip elem allPrimes

-- Compiles, but not working
allPrimes :: [Integer]
allPrimes = primeGen [2..] where
    primeGen :: [Integer] -> [Integer]
    primeGen []     = []
    primeGen (x:xs) = x : primeGen (filter (notDivisibleBy x) xs)

-- filter (\x -> x < 10000 && x > 9000) allPrimes

notDivisibleBy :: Integer -> Integer -> Bool
notDivisibleBy d n = (mod n d) /= 0

-- myPrimes :: [Integer]
-- myPrimes = 2:foldl (flip $ filter . nDB2) [3..] allPrimes

-- nDB2 :: Integer -> Integer -> Bool
-- nDB2 d n = n == d || (mod n d) /= 0

-- dot :: (c -> d) -> (a -> b -> c) -> a -> b -> d
-- dot f g x1 x2 = f $ g x1 x2



decomposite :: Integer -> [Integer]
decomposite x = reverse $ decompositeAccu x [] allPrimes
    where 
        -- decompositeAccu :: Int -> [Int] -> [Integer] -> [Int]
        decompositeAccu 1 accu _  = accu
        decompositeAccu x accu (p:ps)
            | x < 1          = error "x must be positive!"
            | x `mod` p == 0 = decompositeAccu (x `div` p) (p:accu) (p:ps)
            | otherwise      = decompositeAccu x accu ps

-- Problem 32
-- (**) Greates common divisor
gcdEuclid :: Int -> Int -> Int
gcdEuclid 0 gcd = gcd
gcdEuclid x y   = gcdEuclid (mod y x) x

-- Problem 33
-- (*) Determine whether two integers are coprime
coprime :: (Int, Int) -> Bool
coprime = ((==) 1) . (uncurry gcd)


-- Problem 34
-- (**) Calculate Euler's totient function phi(m). 
phi :: Int -> Int
phi m = let r = m - 1
            cases = zip [1..r] $ repeat m
        in  length $ filter coprime cases 

-- Problem 35
factors = decomposite


-- Problem 40
-- (**) Goldbach's conjecture.
goldbach :: Integer -> (Integer, Integer)
goldbach x = findDiv x $ take (piX x) allPrimes where
    findDiv :: Integer -> [Integer] -> (Integer, Integer)
    findDiv x (d:ds) = if (elem (x - d) (d:ds)) then (d, x-d)
                       else (findDiv x ds)

piX :: Integer -> Int
piX x = max (round $ sqrt x') (ceiling $ x' / (logBase 3 x'))
    where x' = fromIntegral x


-- Problem 41
-- (**) Given a range of integers by its lower and upper limit, 
-- print a list of all even numbers and their Goldbach composition. 


-- ### Logic and Codes
-- Problem 46
-- (**) define logical predicates
xor' :: Bool -> Bool -> Bool
xor' = ((.).(.)) not (==)

xor'' :: Bool -> Bool -> Bool
xor'' = (/=)


-- (**) print truth table of a given logical expression
-- Suggested answer:
table :: (Bool -> Bool -> Bool) -> IO ()
table f = putStrLn $ concatMap (++ "\n" )
          [show a ++ " " ++ show b ++ " " ++ show (f a b)
          | a <- [True, False], b <- [True, False] ]

-- Extension: table of screen sizes
data Length = Inch Double | Millimeter Int
    deriving (Eq)
-- TODO: instantiate Show
instance Show Length where
  show (Millimeter m) = show m ++ "mm"
  show len            = show (toMM len)


toInch :: Length -> Length
toInch (Inch i)       = Inch i
toInch (Millimeter m) = Inch ((fromIntegral m) / 25.4) 

toMM :: Length -> Length
toMM (Millimeter m) = Millimeter m
toMM (Inch i)       = Millimeter $ floor (i * 25.4)

-- The returned Double does not enforce data integrity
fromLength :: Length -> Double
fromLength (Inch i) = i
fromLength len      = fromLength $ toInch len

data Ratio = Ratio Int Int
instance Show Ratio where
  show (Ratio w h) = "@r" ++ show w ++ "/" ++ show h 

r9, r10 :: Ratio
r9  = Ratio 16 9
r10 = Ratio 16 10

convertR :: Ratio -> [Double] -> [Ratio] -> IO()
convertR r0 ds rs = convertRatio r0 (map Inch ds) rs


convertRatio :: Ratio -> [Length] -> [Ratio] -> IO()
convertRatio r0 diagonals ratios =
    putStrLn $ concatMap (++ "\n")
    [show d ++ show r0 ++ "\t-> " ++ show (alterRatio d r0 r) ++ show r
     | d <- diagonals, r <- ratios]

alterRatio :: Length -> Ratio -> Ratio -> Length
alterRatio len (Ratio w0 h0) (Ratio w1 h1) =
    let d   = fromLength len
        w0' = fromIntegral w0
        w1' = fromIntegral w1
        h0' = fromIntegral h0
        h1' = fromIntegral h1
        d0  = sqrt (w0'^2 + h0'^2)
        d1  = sqrt (w1'^2 + h1'^2)
        d2  = d / d0 * h0' / h1' * d1
    in  Inch d2

-- Problem 48
-- (**) generalize the table function so that the expression can contain
-- an arbitrary number of logical variables.
tablen :: Int -> ([Bool] -> Bool) -> IO()
tablen n f = putStrLn $ concatMap (++ "\n")
             [concatMap (\x -> (show x ++ " ")) p ++ show (f p) |
              p <- param $ replicate n [True, False]]
param :: [[Bool]] -> [[Bool]]
param []             = [[]]
param (space:spaces) = [(x:xs) | x <- space, 
                                xs <- param spaces]


-- table :: (Bool -> Bool -> Bool) -> IO ()
-- table f = putStrLn $ concatMap (++ "\n" )
--           [show a ++ " " ++ show b ++ " " ++ show (f a b)
--           | a <- [True, False], b <- [True, False] ]

-- Problem 49
-- (**) Gray Codes
-- > gray 3
-- ["000","001","011","010","110","111","101","100"]

permu :: Int -> [String]
permu = permuBy [0, 1]
-- Pitfall: do not use "01" as
-- > show '0'
-- "'0'"

permuBy :: (Show a) => [a] -> Int -> [String]
permuBy = ((.)$(.).(.)) map (concatMap show) paramPower
-- equivalent to
-- permuBy space n = map (concatMap show) $ paramPower space n

-- A refactored version of `param`
paramPower :: (Show a) => [a] -> Int -> [[a]]
paramPower space 0 = [[]]
paramPower space n = [(x:xs) | x <- space,
                              xs <- paramPower space (n-1)]

-- CORRECT solution:
gray :: Int -> [String]
gray 0 = [""]
gray n = let xs = gray (n-1) in map ('0':) xs ++ map ('1':) (reverse xs)
-- similar solution using list comprehension
-- gray n = ['0':rest | rest <- prev] ++
--          ['1':rest | rest <- reverse prev]
--          where prev = gray $ n - 1

-- Problem 50
-- (***) Huffman Codes
-- huffman :: (Eq a) => [(a, Int)] -> [(a, String)]

-- *How to define the following type?*
-- data MyNode a = MyNode a
-- instance Ord (MyNode a) where
--     compare (MyNode x) (MyNode y) = compare x y




