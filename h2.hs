--
-- h2.hs
-- @author Sidharth Mishra
-- @description Testing out Haskell's higher order functions
-- @copyright 2017 Sidharth Mishra
-- @created Sun Oct 22 2017 16:52:56 GMT-0700 (PDT)
-- @last-modified Sun Oct 22 2017 16:52:56 GMT-0700 (PDT)
--

makeListOfAdders :: [Integer] -> [Integer -> Integer]
-- makes a list of adders, without using map
-- makeListOfAdders [] = []
-- makeListOfAdders (x:xs) = (+ x) : makeListOfAdders xs

-- using map
makeListOfAdders = map (+) 

myMap :: (a -> b) -> [a] -> [b]
-- my map function
myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs

myFoldl :: (b -> a -> b) -> b -> [a] -> b
-- My left fold
myFoldl _ acc [] = acc
myFoldl f acc (x:xs) = myFoldl f (f acc x) xs

myFoldr :: (a -> b -> b) -> b -> [a] -> b
-- My right fold
myFoldr _ acc [] = acc
myFoldr f acc (x:xs) = f x (myFoldr f acc xs)

myFoldl' :: (b -> a -> b) -> b -> [a] -> b
-- My left fold in terms of right fold
myFoldl' _ acc [] = acc
-- myFoldl' f acc (x:xs) = myFoldr (\a b -> f b a) acc (x:xs)
-- alternative solution using flip
myFoldl' f acc (x:xs) = myFoldr (flip f)  acc (x:xs)

-- Testing my map
myMapSum = myMap (+ 1) [1,2]
-- Testing myFoldl
myLFoldedSum = myFoldl (+) 0 [1..5000000]
-- Testing myFoldr
myRFoldedSum = myFoldr (+) 0 [1..5000000]
-- Testing myFoldl'
myLRFoldedSum = myFoldl' (+) 0 [1..5000000]

-- define a function to reverse a list using foldl
myreverse :: [a] -> [a]
myreverse = myFoldl (flip (:)) [] -- using flip on demand of the linter
-- myreverse = myFoldl (\acc x -> x : acc) []
-- alternative using `flip` function
-- myreverse = myFoldl (flip (:)) []

-- Testing out fmap
myFMap = fmap (+1) [1,2,3,4]