import Data.List

-- showMtree :: Show a => Mtree a -> String
data Mtree a = Mnode a [Mtree a]
showMtree :: Show a => Mtree a -> String
showMtree (Mnode v []) = show v ++ "\n"
showMtree (Mnode v xs) = (show v) ++ "\n"
                         ++ foldl (++) [] (map (((++) " ") . showMtree) xs)


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

-- PointFree
-- ?????????
-- mySum = foldl (+) 0

-- Problem 16
-- (**) Drop every N'th element from a list.
dropNth :: [a] -> Int -> [a]
dropNth xs n
    | n > (length xs) = xs
    | otherwise       = (init $ take n xs) ++ (dropNth (drop n xs) n)
-- One Suggested Solution:
-- dropEvery xs n = map fst $ filter ((n/=) . snd) $ zip xs (cycle [1..n])
-- [Lesson] filter | map approaches do not suit this problem, unless like above.

-- 9 Problem 19
-- (**) Rotate a list N places to the left.
rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate xs n = (snd xs') ++ (fst xs')
    where n'  = mod ((mod n l) + l) l
          l   = length xs
          xs' = splitAt n' xs

-- 10 Problem 20
-- (*) Remove the K'th element from a list.
removeAt :: [a] -> Int -> [a]
removeAt xs n = map fst $ filter ((/=n).snd) (zip xs [1..])





