module Chapter3.AnnonymousFunctions where
import Chapter2.DataTypes

isEqualTo :: Int -> (Int -> Bool)
isEqualTo a = \x -> x==a 

filterOnes :: [Int] -> [Int]
filterOnes items = filter (isEqualTo 1) items

filterANumber :: [Int] -> Int -> [Int]
filterANumber items number = filter (isEqualTo number) items

filterNot :: (a -> Bool) -> [a] -> [a]
filterNot condition xs = filter (\x -> not (condition x)) xs

isGovOrg :: Client -> Bool
isGovOrg (GovOrg _) = True
isGovOrg _ = False

filterGovOrgs :: [Client] -> [Client]
filterGovOrgs clients = filter (\x -> case x of 
                                        (GovOrg _) -> True
                                        _ -> False
                               ) clients


