-- - Q1 Tree Sort
data Tree a = Empty | Node (Tree a) a (Tree a)
makebst :: (Ord a) => a -> Tree a -> Tree a
makebst v Empty = Node Empty v Empty
makebst x (Node l v r)
    | x <= v        = Node (makebst x l) v r
    | x > v         = Node l v (makebst x r)

insertList :: (Ord a) => [a] -> Tree a -> Tree a
insertList [] t     = t
insertList (x:xs) t = 
                      let newT = makebst x t in
                      insertList xs newT
                      
toList :: Tree a -> [a]
toList Empty = []
toList (Node l v r) = (toList l) ++ [v] ++ (toList r)




-- + Q2 Transpose by Bob
transpose :: [[a]] -> [[a]]
transpose ([] : _) = []
transpose xs       = (map head xs):transpose (map tail xs) 

-- suggested solution
-- >transpose' len [] = replicate len []
-- >transpose' len (xs:xss)
-- >  | len == length xs = zipWith (:) xs (transpose' len xss)
-- >  | otherwise = error "transpose of non-rectangular matrix"



-- + Q3 length, sum & sum of square
sumList :: (Num a) => [a] -> (a, a, a)
sumList []     = (0, 0, 0)
sumList (x:xs) = (a + 1, b + x, c + x*x)
    where (a, b, c) = sumList xs