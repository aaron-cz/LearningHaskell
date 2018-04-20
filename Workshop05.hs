import Data.Maybe

-- Q1: maybeApply [fmap]
maybeApply :: (a -> b) -> Maybe a -> Maybe b
maybeApply f Nothing  = Nothing
maybeApply f (Just x) = Just (f x)
 
-- Q2: zWith [zipWith]
zWith :: (a->b->c) -> [a] -> [b] -> [c]
zWith f []  [] = []
zWith f []  _  = []
zWith f _   [] = []
zWith f (x:xs) (y:ys) = [f x y] ++ zWith f xs ys

-- Q3: linearEqn
linearEqn :: Num a => a -> a -> [a] -> [a]
linearEqn slope inter x = map (((+) inter).((*) slope)) x

-- Q4: allSqrts
allSqrts :: (Floating a, Ord a) => [a] -> [a]
allSqrts x = foldl (++) [] (map sqrtPM x)
sqrtPM :: (Floating a, Ord a) => a -> [a]
sqrtPM x
  | x  > 0    = let y = sqrt x in [y, -y] 
  | x == 0    = [0]
  | otherwise = []

-- Q5 by Bob
-- ?????????
sqrtNN :: (Floating a, Ord a) => [a] -> [a]
sqrtNN xs = map (fromJust) (filter (not . isNothing) (map sqrt' xs))
    where sqrt' :: (Floating a, Ord a) => a -> Maybe a
          sqrt' a
            | a >= 0 = Just (sqrt a)
            | a <  0 = Nothing
sqrtFold :: (Floating a, Ord a) => [a] -> [a]
sqrtFold xs = foldr (maybeAppend) [] xs
    where maybeAppend :: (Floating a, Ord a) => a -> [a] -> [a]
          maybeAppend a xs
            | a >= 0     = (sqrt a) : xs
            | otherwise  = xs