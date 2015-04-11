
module Chapter3.ParametricPolymorphism where

swapTriple ::(t,t1,t2) -> (t1,t2,t)
swapTriple (x,y,z) = (y,z,x)

nothing :: a -> Maybe a
nothing _ = Nothing

maybeA ::[t] -> Char
maybeA [] = 'a'

index ::  [t] -> [(Int, t)]
index [] = []
index [x] = [(0,x)]
index (x:xs) = let (n,_):_ = index xs
               in (n+1,x):(index xs)

