module Chapter2.Guards where

ackermann :: Int -> Int -> Int
ackermann 0 n  = n+1
ackermann m 0 | m > 0 = ackermann (m-1) 1
ackermann m n = ackermann (m-1) (ackermann m (n-1))

myUnzip :: [(Int,Int)] -> ([Int],[Int])
myUnzip [] = ([],[])
myUnzip (t:ts) = ((fst t): (fst (myUnzip ts)),(snd t): (snd (myUnzip ts)))
