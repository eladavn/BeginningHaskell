module Chapter3.DivingIntoLists where

myFilter:: (a -> Bool) -> [a]  -> [a]
myFilter _ [] =  []
myFilter f (x:xs) 
    | f x       = x:myFilter f xs
    | otherwise = myFilter f xs
    
myFoldl :: (item -> acc ->  acc) ->  acc -> [item] -> acc
myFoldl _ acc [] = acc
myFoldl f acc (item:items) = myFoldl f (f item acc) items 
  
    
    
    

