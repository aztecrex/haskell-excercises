module Excercises where

import Control.Monad(foldM)

class Fluffy f where
  furry :: (a -> b) -> f a -> f b

-- Exercise 1
-- Relative Difficulty: 1
instance Fluffy [] where
  furry = map

-- Exercise 2
-- Relative Difficulty: 1
instance Fluffy Maybe where
  furry f (Just x)= Just $ f x
  furry _ Nothing = Nothing

-- Exercise 3
-- Relative Difficulty: 5
instance Fluffy ((->) t) where
  furry f g x = f $ g x

newtype EitherLeft b a = EitherLeft (Either a b) deriving (Eq, Show)
newtype EitherRight a b = EitherRight (Either a b) deriving (Eq, Show)

-- Exercise 4
-- Relative Difficulty: 5
instance Fluffy (EitherLeft t) where
  furry f (EitherLeft (Left v)) = EitherLeft (Left $ f v)
  furry f (EitherLeft (Right v)) = EitherLeft (Right v)

-- Exercise 5
-- Relative Difficulty: 5
instance Fluffy (EitherRight t) where
  furry f (EitherRight (Right v)) = EitherRight (Right $ f v)
  furry f (EitherRight (Left v)) = EitherRight (Left v)


class Misty m where
  banana :: (a -> m b) -> m a -> m b
  unicorn :: a -> m a



  -- Exercise 6
  -- Relative Difficulty: 3
  -- (use banana and/or unicorn)

furry' :: (Misty m) => (a -> b) -> m a -> m b
furry' f = banana $ unicorn . f

-- Exercise 7
-- Relative Difficulty: 2
instance Misty [] where
  banana = concatMap
  unicorn x = [x]

-- Exercise 8
-- Relative Difficulty: 2
instance Misty Maybe where
  banana f (Just x) = f x
  banana _ Nothing = Nothing
  unicorn = Just

-- Exercise 9
-- Relative Difficulty: 6
instance Misty ((->) t) where
  banana f mx a = f (mx a) a
  unicorn = const

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

-- Exercise 13
-- Relative Difficulty: 6
apple :: (Misty m) => m a -> m (a -> b) -> m b
apple ma mf = banana (\a -> (banana (\f -> unicorn (f a)) mf ) ) ma


-- Exercise 14
-- Relative Difficulty: 6

-- like foldM
foldy :: (Misty m) => (b -> a -> m b) -> b -> [a] -> m b
foldy _ a [] = unicorn a
foldy f a (x:xs) = (\ax -> foldy f ax xs) `banana` f a x

moppy :: (Misty m) => [a] -> (a -> m b) -> m [b]
moppy as mf = furry' reverse $
  foldy (\bs a -> banana (\b -> unicorn (b:bs)) $ mf a ) [] as

-- Exercise 15
-- Relative Difficulty: 6
-- (bonus: use moppy)
sausage :: (Misty m) => [m a] -> m [a]
sausage ms = moppy ms id

-- Exercise 16
-- Relative Difficulty: 6
-- (bonus: use apple + furry')


banana2 :: (Misty m) => (a -> b -> c) -> m a -> m b -> m c
banana2 f m1 m2 = apple m2 $ furry' f m1

-- Exercise 17
-- Relative Difficulty: 6
-- (bonus: use apple + banana2)
banana3 :: (Misty m) => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
banana3 f m1 m2 m3 = apple m3 $ banana2 f m1 m2

-- Exercise 18
-- Relative Difficulty: 6
-- (bonus: use apple + banana3)
banana4 :: (Misty m) => (a -> b -> c -> d -> e) -> m a -> m b -> m c -> m d -> m e
banana4 f m1 m2 m3 m4 = apple m4 $ banana3 f m1 m2 m3

newtype State s a = State {
  state :: s -> (s, a)
}

-- Exercise 19
-- Relative Difficulty: 9
instance Fluffy (State s) where
  furry f s1 = State (\s -> (s, f (snd (state s1 s))))

-- Exercise 20
-- Relative Difficulty: 10
instance Misty (State s) where
  banana = error "todo"
  unicorn x = State (\s -> (s,x))
