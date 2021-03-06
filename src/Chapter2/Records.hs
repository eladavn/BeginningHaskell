module Chapter2.Records where

data ClientR = 
     GovOrgR { clientRName :: String }
   | CompanyR { clientRName :: String
                , companyId :: Integer
                , person :: PersonR
                , duty :: String }
   | IndividualR { person :: PersonR } 
   deriving Show
   
data PersonR = PersonR { firstName :: String
                        , lastName :: String } 
   deriving Show

