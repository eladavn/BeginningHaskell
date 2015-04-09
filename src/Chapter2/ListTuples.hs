module Chapter2.ListTuples where

sorted :: [Integer] -> Bool
sorted [] = True
sorted [_] = True
sorted (x : r@(y:_)) = x < y && sorted r

maxmin :: [Int] -> (Int,Int)
maxmin [x] = (x,x)
maxmin (x:xs) = ( if x > xs_max then x else xs_max, if x < xs_min then x else xs_min) where (xs_max, xs_min) = maxmin xs

ifibonacci :: Integer -> Maybe Integer
ifibonacci 0 = Just 0
ifibonacci 1 = Just 1
ifibonacci n 
    | n < 0 = Nothing
    | otherwise = let (Just f1, Just f2) = (ifibonacci (n-1), ifibonacci (n-2))
                           in Just (f1 + f2)
--ifibonacci n = if n < 0
--    then Nothing
--    else case n of
--    0 -> Just 0
--    1 -> Just 1
--    num -> let Just f1 = ifibonacci (num-1)
--               Just f2 = ifibonacci (num-2)
--         in Just (f1 + f2)