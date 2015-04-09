module Chapter2.SimpleFunctions where

firstOrEmpty :: [String] -> String
firstOrEmpty lst = if not (null lst) then head lst else "empty"

(+++) :: [a] -> [a] -> [a]
lst1 +++ lst2  = if  null lst1 then lst2 else head lst1 : (tail lst1 +++ lst2)  

--A  commont
reverse2 :: [t] -> [t]
reverse2 lst =  if null lst then [] else reverse2 (tail lst) +++ [head lst] 

                    


