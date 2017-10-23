{-# LANGUAGE TypeOperators, KindSignatures #-}
--
-- h3.hs
-- @author Sidharth Mishra
-- @description Testing out Functors, Applicative Functors and Monads
-- @copyright 2017 Sidharth Mishra
-- @created Sun Oct 22 2017 17:16:35 GMT-0700 (PDT)
-- @last-modified Sun Oct 22 2017 17:16:35 GMT-0700 (PDT)
--

import Prelude hiding ((<*>), (<$>), Functor, Applicative, fmap, pure, (-:))
import Control.Applicative (liftA2)
import qualified Data.Map as Map

x -: f = f x

-- myadd x y = y + x
-- mysub x y = y - x
-- mymul x y = y * x
-- mydiv d = (\n -> case d of
--   0 -> error "div by 0"
--   d -> div n d)

-- Rewritten using Maybe Monad
myadd x y = Just $ y + x
mysub x y = Just $ y - x
mymul x y = Just $ y * x
mydiv d n = case d of
  0 -> Nothing
  d -> Just $ div n d

-- a foo function for doing div, mult using `do`
foo x = do
  y <- mydiv 60 x
  z <- myadd 3 9
  mymul y z

-- My functor
-- A functor is something that can be mapped over
-- It acts as a container/box
class MyFunctor (f :: * -> *) where
  fmap' :: (a -> b) -> f a -> f b
  (<$>) :: (a -> b) -> f a -> f b
  (<$>) = fmap' -- <$> is the infix operator for fmap

-- My functor for a Maybe type(Maybe is declared with data keyword)
-- types have kinds, since Maybe if defined as
-- data Maybe a = Nothing | Just a
-- it has kind * -> *
-- that means, it has 1 type variable in its definition
-- where as a type like String has kind *
-- that means it has no type variables in its definition
instance MyFunctor Maybe where
  fmap' _ Nothing = Nothing
  fmap' f (Just x) = Just $ f x

-- Incase of Either, it has a kind of (* -> * -> *)
-- since it is defined as
-- data Either a b = Left a | Right b
-- As Either takes 2 type variables for definition, its kind becomes (* -> * -> *)
-- which signifies the kind [Either :: a -> b -> (Either a b)]
-- now, the constraint on MyFunctor is that its type variable
-- `f` must have a kind (* -> *), so we need to partially apply `Either`
-- with one type variable so that it's kind becomes (* -> *)
-- parially applying Either with type variable gives us kind (Either a :: * -> *)
-- which fits the MyFunctor's definition and we can instantiate it for (Either a)
instance MyFunctor (Either a) where
  fmap' _ (Left x) = Left x
  fmap' f (Right x) = Right $ f x


-- Functor for IO action
-- IO has kind (IO :: * -> *)
-- IO is a functor
instance MyFunctor IO where
  fmap' f action = do
    result <- action
    return $ f result


-- Haskell's Functor laws are not enforced but as beneficial for reasoning about the code
-- Identity law
-- fmap id x = x
-- Ditributive law
-- fmap (f . g) = (fmap f) . (fmap g)
-- or
-- fmap (f (g)) x = (fmap f (fmap g x))

-- one way to get function out of Maybe Functor
-- and apply the function on incoming parameter
-- To simplify this, we use Control.Applicative
-- Applicative is the Applicative Functor
wrappedFuction :: Maybe (b -> b) -> b -> b
wrappedFuction t x = case t of
  (Just f) -> f x
  Nothing -> error "bbb"


-- What is a functor?
-- A functor is something that can be mapped over.
-- fmap :: Functor f => (a -> b) -> f a -> f b

-- What is an applicative functor?
-- A functor that you can apply to other functors.
-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b

-- Applicative Functor is defined as
-- The part before `=>` is the constraint
-- That means forall `f` such that f is a `Functor`
-- and has kind (f :: * -> *)
-- we define Applicative f as having the functions
-- pure, (<*>), (*>), (<*)
-- class Functor f => Applicative (f :: * -> *) where
--   pure :: a -> f a -- same as return
--   (<*>) :: f (a -> b) -> f a -> f b -- same as fmap but applies over functors containing functions, i.e the function is itself inside the functor, we are applying values to it from outside
-- using the <*> Applicative functor's operator or the `starship operator`
--   (*>) :: f a -> f b -> f b
--   (<*) :: f a -> f b -> f a

-- Applicative Functor laws - not enforced, but good to follow
-- 1> pure f <*> x = fmap f x
-- 2> pure id <*> v = v
-- 3> pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
-- 4> pure f <*> pure x = pure (f x)
-- 5> u <*> pure y = pure ($ y) <*> u

-- MyApplicative type class definition
class MyFunctor f => MyApplicative (f :: * -> *) where
  pure :: a -> f a -- pure is just like return, it wraps the value into Applicative functor's base type `f`
  (<*>) :: f (a -> b) -> f a -> f b
  (*>) :: f a -> f b -> f b
  (<*) :: f a -> f b -> f a

-- defining the instance for Maybe functor
instance MyApplicative Maybe where
  -- pure
  pure = Just
  
  -- <*>
  -- (Just f) <*> (Just x) = Just $ f x
  -- _ <*> Nothing = Nothing
  -- Nothing <*> _ = Nothing
  Nothing <*> _ = Nothing
  (Just f) <*> x = fmap' f x
  -- (Just f) <*> x = f <$> x

  -- *>
  _ *> x = x
  
  -- <*
  x <* _ = x

-- MyFunctor instance for [] -- list type
-- [] is defined as:
-- data [] a = [] | a : [a] 
instance MyFunctor [] where
  fmap' = map

-- my applicative instance for list
instance MyApplicative [] where
  pure = (:[])
  fs <*> xs = [ f x | f <- fs, x <- xs ]
  _ *> bs = bs
  as <* _ = as

-- testing my applicative functor
myAppFunc[] = (*) <$> [1,2,3] <*> [1,0,0,1] -- gives [1,0,0,1,2,0,0,2,3,0,0,3]
pure7 = pure 7 :: [Int] -- gives [7]

-- instantiating IO for MyApplicative
instance MyApplicative IO where
  pure = return
  a <*> b = do
    f <- a
    x <- b
    return $ f x
  a <* _ = a
  _ *> b = b

-- testing my applicative IO
myAppIO :: IO ()
myAppIO = do
  a <- (++) <$> getLine <*> getLine
  putStrLn a


-- import Control.Applicative
-- ghci> :t liftA2
-- liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
-- The liftA2 function lets us apply a normal
-- function to two functors more easily.
-- It is defined as:
-- liftA2 f a b = f <$> a <*> b
-- it takes a normal funtion, uses fmap over a to give us a functor of function
-- then uses the `starship` or applicative functor operator to apply the
-- functor containing function with next functor to give out resulting functor

-- myAppIO using liftA2
myAppIO2 :: IO ()
myAppIO2 = do
  a <- liftA2 (++) getLine getLine
  putStrLn a


-- Monoid is an associative binary function and a value
-- that acts as an identity with respect to that function
-- for example
-- (* 1) -- `*` is the multiplication function for Nums and 1 is the value
-- together they act as an identity, so (* 1) is a Monoid
-- (+ 0) is the same
-- (++ []) is the same as well
class MyMonoid m where
  mempty' :: m
  mappend' :: m -> m -> m
  mconcat' :: [m] -> m
  mconcat' = foldr mappend' mempty' -- defined in points free style
  -- since foldr has type foldr :: (a -> b -> b) -> b -> [a] -> b
  -- to be exact foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b

-- Rules for Monoid
-- the `mappend` function of the MyMonoid is Commutative and Associative
-- 1. mempty `mappend` x = x -- Commutative #1
-- 2. x `mappend` mempty = x -- Commutative #2
-- 3. (x `mappend` y) `mappend` z = x `mappend` (y `mappend` z) -- Associative



-- Monad
-- Solves the problems that arise when trying to chain applicative functors
-- like:
-- Just (+3) <*> Just 4 -- works
-- Just (+3) <*> Just (+4) <*> Just 5 -- doesn't work

-- Monads can chain through a series of functions:
-- Just 3 >>= (\x -> Just (x+4))
--        >>= (\x -> Just (x+5))

-- Or equivalently
-- return 3 >>= (\x -> return (x+4))
--          >>= (\x -> return (x+5))

-- A Monad(MyMonad in my case) is defined as:
-- where (>>=) is the bind operator and can be used to chain
class MyApplicative m => MyMonad (m :: * -> *) where
  return' :: a -> m a -- has the same type as `pure` in Applicative functor type class
  (>>==) :: m a -> (a -> m b) -> m b
  (>>>) :: m a -> m b -> m a -- simply skips the value
  fail' :: String -> m a


-- instantiating MyMonad for Maybe
instance MyMonad Maybe where
  return' = Just
  (Just x) >>== f = f x
  Nothing >>== _ = Nothing
  x >>> _ = x
  fail' _ = Nothing

-- testing my Maybe MyMonad

x -:: f = f x

type Pos = (Int, Int)

start :: Pos
start = (0,0)

beerPos = Map.empty
  -:: Map.insert (0,2) True
  -:: Map.insert (-1,3) True
  -:: Map.insert (-3,-8) True

moveTo :: Pos -> Maybe Pos
moveTo p = if Map.member p beerPos
  then Nothing
  else Just p

up (x, y) = moveTo (x, y+1)
down (x, y) = moveTo (x, y-1)
left (x, y) = moveTo (x-1, y)
right (x, y) = moveTo (x+1, y)

-- Just (-1, 0)
track1 = (return start) >>= up >>= left >>= left >>= right >>= down

-- Nothing
track2 = return start >>= left >>= left >>= up >>= up >>= right >>= up >>= right >>= right >>= down

-- `do` block is the syntactic sugar for Monad (>>=) operator chaining
-- it seems natural only in certain cases
-- when there are many Monad values we need to compute at the same time
-- like:
mydivMaybeMonads x y = x >>= (\numer ->
    y >>= (\denom ->
      if denom > 0 
        then Just $ numer `div` denom
        else fail "div by 0"
    )
  )

-- simplified using do syntax
-- but this becomes troublesome when solving something
-- like track2 or track1 where you need immediate chaining
-- then using (>>=) seems to be better choice
mydivMaybeMonads' x y = do
  x' <- x
  y' <- y
  case y' of
    0 -> fail "div by 0"
    y' -> Just $ div x' y'


-- MyMonad implementation for []
-- used in list comprehensions
-- for eg: [(n,ch) | n <- [1,2], ch <- ['a','b']]
instance MyMonad [] where
  return' x = [x]
  xs >>== f = concatMap f xs
  -- xs >>== f = concat (map f xs) -- same as concatMap
  -- basically the (map f xs) will give a container of lists
  -- so the `concat` will concatenate all the elements of the container to give back a list
  fail' _ = []
  xs >>> _ = xs  


-- testing List monad
-- using bind operator

-- How the example(myTestLstMonad) below works:
-- for [1,2]
-- the lambda f is mapped over it and we get [f 1, f 2]
-- Inside the lamda f, g is mapped over ['a', 'b']
-- we get [g x 'a', g x 'b']
-- now f 1 = [(1, 'a'), (1, 'b')]
-- and f 2 = [(2, 'a'), (2, 'b')]
-- so we have [[(1, 'a'), (1, 'b')], [(2, 'a'), (2, 'b')]] i.e a List of Lists
-- the concat will merge them into a single list as
-- [(1,'a'),(1,'b'),(2,'a'),(2,'b')]
myTestLstMonad = [1, 2] >>== (\x -> 
    ['a', 'b'] >>== (\y -> return' (x, y))
  )

-- using do syntax
myTestLstMonad' = do
  x <- [1,2]
  y <- ['a', 'b']
  return (x,y)

-- using list comprehension
-- list comprehensions: syntactic sugar for using lists as monads.
myTestLstMonad'' = [(n,ch) | n <- [1,2], ch <- ['a','b']]