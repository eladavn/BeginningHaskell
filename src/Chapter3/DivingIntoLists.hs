module Chapter3.DivingIntoLists where

myFilter:: (a -> Bool) -> [a]  -> [a]
myFilter _ [] =  []
myFilter f (x:xs) 
    | f x       = x:myFilter f xs
    | otherwise = myFilter f xs
    
myFoldl :: (item -> acc ->  acc) ->  acc -> [item] -> acc
myFoldl _ acc [] = acc
myFoldl f acc (item:items) = myFoldl f (f item acc) items 
  
myProduct :: [Int] -> Int
myProduct xs  = foldl (*) 1 xs

findShortest :: [String] -> Maybe String
findShortest []  = Nothing
findShortest [x] = Just x
findShortest (x:y:xs)
    |  length x < length y = findShortest (x:xs)
    |  otherwise = findShortest (y:xs)
    
shortest :: String -> String -> String
shortest x y
    | length x < length y = x
    | otherwise = y

findShortest' :: [String] -> Maybe String
findShortest' []  = Nothing
findShortest' xs = Just $ foldl1 shortest xs 

myAll :: [Bool] -> Bool
myAll [x]  = x
myAll (x:xs) = myAll [x] && myAll xs 

myAll' :: [Bool] -> Bool
myAll' xs = foldl1 (&&) xs
   
 
    
    

