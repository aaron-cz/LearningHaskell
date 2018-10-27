-- Q2
-- ftoc :: Double -> Double
ftoc f = (5/9) * (f - 32)


-- Q3
quadRoots a b c
    | delta <  0  = []
    | delta == 0  = [-0.5*b/a]
    | delta >  0  = [((-b) - sqrt(b^2 - 4*a*c))/(2*a),
                     ((-b) + sqrt(b^2 - 4*a*c))/(2*a)]
    where delta = b*b - 4*a*c

-- Q4 merge lists

-- Q5 by Bob
-- Data.List 'sort'
quickSort :: (Ord a) => [a] -> [a]
quickSort []       = []
quickSort (x:xs)   = (quickSort left) ++ [x] ++ (quickSort right)
    where 
        left  = filter (<  x) xs
        right = filter (>= x) xs

-- Q6 same_shape by Bob
-- given data structure:
data Tree k v = Leaf | Node k v (Tree k v) (Tree k v)
     deriving (Eq, Show)

same_shape :: Tree k v -> Tree k' v' -> Bool
same_shape Leaf Leaf            = True
same_shape Leaf (Node _ _ _ _)  = False
same_shape (Node _ _ _ _) Leaf  = False
same_shape (Node _ _ l r) (Node _ _ l' r')
            = (same_shape l l') && (same_shape r r')

-- Q7 eval Expression
data Expression = Var Variable
                | Num Integer
                | Plus Expression Expression
                | Minus Expression Expression
                | Times Expression Expression
                | Div Expression Expression

data Variable = A | B

eval :: Integer -> Integer -> Expression -> Integer
eval a b (Var A) = a
eval a b (Var B) = b
eval a b (Num n) = n
eval a b (Plus  x y) = (eval a b x) + (eval a b y)
eval a b (Minus x y) = (eval a b x) - (eval a b y)
eval a b (Times x y) = (eval a b x) * (eval a b y)
eval a b (Div   x y) = div (eval a b x) (eval a b y)