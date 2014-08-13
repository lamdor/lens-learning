{-# LANGUAGE RankNTypes #-}
module Lens where

-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b

type Lens s a = Functor f => (a -> f a) -> s -> f s

newtype Identity a = Identity { runIdentity :: a }

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

over :: Lens s a -> (a -> a) -> s -> s
over ln f = runIdentity . ln (Identity . f)

newtype Const a b = Const { getConst :: a }

instance Functor (Const a) where
  fmap _ (Const a) = Const a

view :: Lens s a -> s -> a
view ln s = getConst $ ln Const s

set :: Lens s a -> a -> s -> s
set ln a = over ln (const a)

_1 :: Functor f => (a -> f a) -> (a,b) -> f (a,b)
_1 f (x, y) = fmap (\x' -> (x', y)) $ f x

data User = User { name :: String, age :: Int } deriving Show
data Project = Project { owner :: User } deriving Show

nameLens :: Lens User String
nameLens f user = fmap (\newName -> user { name = newName }) $ f (name user)

ageLens :: Lens User Int
ageLens f user = fmap (\newAge -> user { age = newAge }) $ f (age user)

ownerLens :: Lens Project User
ownerLens f project = fmap (\newOwner -> project { owner = newOwner }) $ f (owner project)

ownerNameLens :: Lens Project String
ownerNameLens = ownerLens.nameLens
