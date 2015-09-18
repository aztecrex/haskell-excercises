module Excercises where

import Control.Monad(foldM)

class Fluffy f where
  furry :: (a -> b) -> f a -> f b

-- Exercise 1
-- Relative Difficulty: 1
instance Fluffy [] where
  furry = fmap

-- Exercise 2
-- Relative Difficulty: 1
instance Fluffy Maybe where
  furry = fmap

-- Exercise 3
-- Relative Difficulty: 5
instance Fluffy ((->) t) where
  furry = fmap

newtype EitherLeft b a = EitherLeft (Either a b)
newtype EitherRight a b = EitherRight (Either a b)

-- Exercise 4
-- Relative Difficulty: 5
instance Fluffy (EitherLeft t) where
  furry f (EitherLeft (Left v)) = EitherLeft (Left $ f v)
  furry f (EitherLeft (Right v)) = EitherLeft (Right v)
  -- furry f (EitherLeft x) = EitherLeft x
  -- furry = error "todo"

-- Exercise 5
-- Relative Difficulty: 5
instance Fluffy (EitherRight t) where
  furry f (EitherRight x) = EitherRight (fmap f x)

ml :: (Monad m) => (a -> b) -> m a -> m b
ml f = \a -> do
  a' <- a
  return (f a')

ml' :: (Monad m) => (a -> b) -> m a -> m b
ml' f = \a -> ( a >>= (\a' -> return (f a')) )

class Misty m where
  banana :: (a -> m b) -> m a -> m b
  unicorn :: a -> m a
  -- Exercise 6
  -- Relative Difficulty: 3
  -- (use banana and/or unicorn)
  furry' :: (a -> b) -> m a -> m b
  furry' f = \a -> banana (\a' -> unicorn (f a') ) a

-- Exercise 7
-- Relative Difficulty: 2
instance Misty [] where
  banana f xs = concat ( fmap f xs )
  unicorn x = [x]

-- Exercise 8
-- Relative Difficulty: 2
instance Misty Maybe where
  banana f (Just x) = f x
  unicorn x = Just x

-- Exercise 9
-- Relative Difficulty: 6
instance Misty ((->) t) where
  banana f mx = \a -> f (mx a) a
  unicorn x = \_ -> x

-- Exercise 10
-- Relative Difficulty: 6
instance Misty (EitherLeft t) where
  banana f (EitherLeft (Left x))  = f x
  banana f (EitherLeft (Right x)) = EitherLeft (Right x)
  unicorn x = EitherLeft (Left x)

-- Exercise 11
-- Relative Difficulty: 6
instance Misty (EitherRight t) where
  banana f (EitherRight (Right x))  = f x
  banana f (EitherRight (Left x)) = EitherRight (Left x)
  unicorn x = EitherRight (Right x)

-- Exercise 12
-- Relative Difficulty: 3
jellybean :: (Misty m) => m (m a) -> m a
jellybean = banana id

{- some experiments to determine how apple should look -}
apple' ma mf = do
  a <- ma
  f <- mf
  return $ f a

apple'' ma mf = ma >>= (\a -> (mf >>= (\f -> return (f a))))

-- Exercise 13
-- Relative Difficulty: 6
apple :: (Misty m) => m a -> m (a -> b) -> m b
apple ma mf = banana (\a -> (banana (\f -> unicorn (f a)) mf ) ) ma


-- Exercise 14
-- Relative Difficulty: 6
{- some experiments to determine the shape of moppy -}

invert :: (Monad m) => [m a] -> m [a]
invert ms = fmap reverse $ foldM comb [] ms
  where comb as ma = ma >>= (\a -> return (a:as))

moppy' :: (Monad m) => [a] -> (a -> m b) -> m [b]
moppy' as mf = invert $ fmap mf as

li :: [Int]
li = [1,2,3]

i2ms :: Int -> Maybe String
i2ms x = Just $ show x

moppied' :: Maybe [String]
moppied' = moppy' li i2ms


-- so here is the Misty version of the above.

-- like foldM
foldy :: (Misty m) => (b -> a -> m b) -> b -> [a] -> m b
foldy _ a [] = unicorn a
foldy f a (x:xs) = (\ax -> foldy f ax xs) `banana` ( f a x )

-- like invert
confusy :: (Misty m) => [m a] -> m [a]
confusy ms = furry' reverse $ foldy combiny [] ms
  where combiny as ma = (\a -> unicorn (a:as)) `banana` ma

-- Finally..... Moppy!
moppy :: (Misty m) => [a] -> (a -> m b) -> m [b]
moppy as mf = confusy $ fmap mf as

-- show that moppy works
moppied :: Maybe [String]
moppied = moppy li i2ms

moppyIsGood = moppied == moppied'

-- Exercise 15
-- Relative Difficulty: 6
-- (bonus: use moppy)
sausage :: (Misty m) => [m a] -> m [a]
sausage ms = moppy ms id
-- sausage = confusy

-- Exercise 16
-- Relative Difficulty: 6
-- (bonus: use apple + furry')
banana2 :: (Misty m) => (a -> b -> c) -> m a -> m b -> m c
banana2 = error "todo"

-- Exercise 17
-- Relative Difficulty: 6
-- (bonus: use apple + banana2)
banana3 :: (Misty m) => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
banana3 = error "todo"

-- Exercise 18
-- Relative Difficulty: 6
-- (bonus: use apple + banana3)
banana4 :: (Misty m) => (a -> b -> c -> d -> e) -> m a -> m b -> m c -> m d -> m e
banana4 = error "todo"

newtype State s a = State {
  state :: (s -> (s, a))
}

-- Exercise 19
-- Relative Difficulty: 9
instance Fluffy (State s) where
  furry = error "todo"

-- Exercise 20
-- Relative Difficulty: 10
instance Misty (State s) where
  banana = error "todo"
  unicorn = error "todo"
