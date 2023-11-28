import qualified Data.Map as Map

type Rule = (String -> Bool)

-- parseRuleString :: String -> Int
parseRuleString s =  restIds
    where
        (idString, (_:rest)) = break (== ':') s
        restIds  = map (\x -> read x::Int) (words rest)
