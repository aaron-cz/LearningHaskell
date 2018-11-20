-- Q1 fibs :: Int -> [Integer]
fibs :: Int -> [Integer]
fibs 0 = []
fibs 1 = [0]
fibs 2 = [0, 1]
fibs n = fibsLast ++ [fibsLast!!(n-3) + fibsLast!!(n-2)]
    where fibsLast = fibs (n-1)

-- Q2 improved fibs with lazyness
fibs2 :: Int -> [Integer]
fibs2 n = take n fibsAll

fibsAll :: [Integer]
fibsAll = (0:1: (zipWith (+) fibsAll (tail fibsAll)))


-- Q3 comment

-- >mergesort xs = repeat_merge_all (merge_consec (to_single_els xs))
-- >
-- INITIALIZATION [ele0, ele1, ..] -> ([ele0]:[ele1]:..)
-- >to_single_els [] = []
-- >to_single_els (x:xs) = [x] : to_single_els xs
-- >
-- MERGE two lists
-- >merge [] ys = ys
-- >merge (x:xs) [] = x:xs
-- >merge (x:xs) (y:ys)
-- >   | x <= y = x : merge xs (y:ys)
-- >   | x >  y = y : merge (x:xs) ys
-- >
-- MERGE bottom-up by 1 level
-- >merge_consec [] = []
-- >merge_consec [xs] = [xs]
-- >merge_consec (xs1:xs2:xss) = (merge xs1 xs2) : merge_consec xss
-- >
-- ITERATE until #lists = 1
-- >repeat_merge_all [] = []
-- >repeat_merge_all [xs] = xs
-- >repeat_merge_all xss@(_:_:_) = repeat_merge_all (merge_consec xss)

-- Q4 Lazyness analysis

-- Q5 print mTree
data Mtree a = Mnode a [Mtree a]
type Line = String

print_mtree' :: Show a => Mtree a -> IO ()
print_mtree' t =
    let
        toLines :: Show a => Mtree a -> [Line]
        toLines (Mnode val cs) = show val : map (' ':) (concatMap (toLines) cs)
    in  foldl (\acc str -> acc >> (putStrLn str)) (return ()) (toLines t)