module Chapter2.PatternMatching where

import Chapter2.DataTypes

g :: Client -> String
g client = case client of
    Company _ _ (Person name _ _) pos -> case pos of 
        "Boss" -> name ++ " is the boss"
        _ -> "There is no boss"

addItemToClientsPerGender :: Client -> (Int, Int) -> (Int, Int)
addItemToClientsPerGender client accumolator = case client of
    Individual person _ ->  case person of
        Person _ _ Male -> (fst accumolator +1, snd accumolator)
        Person _ _ Female -> (fst accumolator, snd accumolator +1)
        Person _ _ Unknown ->  accumolator 

getClientsPerGender :: [Client] -> (Int, Int)
getClientsPerGender clients = if (length clients == 0) 
                              then (0,0) 
                              else addItemToClientsPerGender (head clients) (getClientsPerGender (tail clients))
                              
discountTimeMachine :: TimeMachine -> Float -> TimeMachine
discountTimeMachine tm@(TimeMachine {price = pr}) percentage = tm {price = pr*percentage/100}

discountTimeMachines :: [TimeMachine] -> Float -> [TimeMachine]
discountTimeMachines machines percentage = if null machines 
                                           then []
                                           else (discountTimeMachine (head machines) percentage) : (discountTimeMachines (tail machines) percentage) 


    
    