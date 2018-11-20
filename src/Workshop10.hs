module Workshop10 where

-- import Data.List
import Data.Maybe
import Data.Char
-- Q1 maybe_tail, maybe_drop
maybe_tail :: [a] -> Maybe [a]
maybe_tail (x:xs) = Just xs
maybe_tail []     = Nothing

maybe_drop1 :: Int -> [a] -> Maybe [a]
maybe_drop1 n list
    | (length list) < n = Nothing
    | otherwise         = Just (drop n list)

maybe_drop2 :: Int -> [a] -> Maybe [a]
maybe_drop2 0 list = Just list
maybe_drop2 n list = if (isNothing tlist) then Nothing
                     else (maybe_drop2 (n - 1) (fromMaybe [] tlist))
                     where tlist = maybe_tail list


maybe_drop3 :: Int -> [a] -> Maybe [a]
maybe_drop3 0 xs = Just xs
maybe_drop3 n xs = do
    tail <- maybe_tail xs
    maybe_drop3 (n - 1) tail



-- Q2 print_tree
data Tree a = Empty | Node (Tree a) a (Tree a)
print_tree :: (Show a) => Tree a -> IO()
print_tree Empty = return ()
print_tree (Node l v r) = do
    print_tree l
    putStrLn (show v)
    print_tree r

-- Q3 str_to_num
-- Data.Char.isDigit    :: Char -> Bool
-- Data.Char.digitToInt :: Char -> Int

str_to_num :: String -> Maybe Int
str_to_num s = strToNum s 0

strToNum :: String -> Int -> Maybe Int
strToNum []     accu = Just accu
strToNum (c:cs) accu = do
    d <- maybeToInt c
    strToNum cs (accu * 10 + d)

maybeToInt :: Char -> Maybe Int
maybeToInt c = if (isDigit c) then Just (digitToInt c)
               else Nothing

-- Q4 reads in a list of lines and compute sum
sumLine :: Int -> IO Int
sumLine accu = do
    s <- getLine
    let i = str_to_num s
    if (isNothing i) then return accu
    else sumLine ((fromMaybe 0 i) + accu)
    -- *alternative statement*
    -- case i of 
    --     Nothing  -> return accu
    --     Just num -> sumLine (accu + num)

-- TODO: Q4 Version 2, Q5

-- Version 1
-- >sum_lines :: IO Int
-- >sum_lines = do
-- >   line <- getLine
-- >   case str_to_num line of
-- >       Nothing -> return 0
-- >       Just num -> do
-- >           sum <- sum_lines
-- >           return (num+sum)

-- Version 2
-- >sum_lines' :: IO Int
-- >sum_lines' = do
-- >   nums <- list_num_lines
-- >   return (sum nums)


-- >list_num_lines :: IO [Int]
-- >list_num_lines = do
-- >   line <- getLine
-- >   case str_to_num line of
-- >       Nothing -> return []
-- >       Just num -> do
-- >           nums <- list_num_lines
-- >           return (num:nums)

