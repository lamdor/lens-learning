module NaiveLenses where

data User = User { name :: String
                 , age :: Int
                 } deriving Show
                            
data Project = Project { owner :: User }
             deriving Show

data NaiveLens s a = NaiveLens
                     { view :: s -> a
                     , over :: (a -> a) -> s -> s }

luke = User { name = "Luke", age = 31 }

-- nameLens = NaiveLens name (\a s -> s { name = a } )

ageLens = NaiveLens age (\f s -> s { age = f (age s) } )

set :: NaiveLens s a -> a -> s -> s
set ln a s = over ln (const a) s




