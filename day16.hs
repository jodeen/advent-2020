import Data.List
import Day16Data

type RuleSpec = (String, (Int, Int), (Int, Int)) 

day16Ticket = [53,101,83,151,127,131,103,61,73,71,97,89,113,67,149,163,139,59,79,137]

sampleDataRules = [
    ("class", (1,3), (5,7)),
    ("row", (6,11), (33,44)),
    ("seat", (13,40),(45,50))] :: [(String, (Int, Int),(Int, Int))]

sampleDataNearby = [
    [7,3,47],
    [40,4,50],
    [55,2,20],
    [38,6,12]] :: [[Int]]

sampleDataRules2 = [
    ("class", (0,1), (4,19)),
    ("row", (0,5), (8,19)),
    ("seat", (0,13),(16,19))] :: [(String, (Int, Int),(Int, Int))]

sampleDataNearby2 = [
    [3,9,18],
    [15,1,5],
    [5,14,9]] :: [[Int]]    

between :: Int -> (Int, Int) -> Bool 
between x (a,b) = x >= a && x <= b

toRule :: (String, (Int, Int),(Int, Int)) -> (Int -> Bool)
toRule (_, a, b) x = (x `between` a) || (x `between` b)

isValid :: [Int -> Bool] -> Int -> Bool 
isValid rules num = any ($ num) rules

part1 rulesList nums = sum (filter (not . isValid rules) (concat nums))
    where
        rules = map toRule rulesList

findValidRule :: [(String, (Int, Int),(Int, Int))] -> [Int] -> [(String, (Int, Int),(Int, Int))]
findValidRule ruleStrings nums = filter (\s -> all (toRule s) nums) ruleStrings 


ticketRules ruleString nearby = map (findValidRule ruleString) (transpose validTickets)
    where
        rules = map toRule ruleString
        validTickets = filter (all (isValid rules)) nearby

doStep :: [[RuleSpec]] -> [[RuleSpec]]
doStep rules = if (all (\x -> length x == 1) rules) then rules else doStep removed
    where
        matched = map head ((filter (\x -> length x == 1)) rules)
        removed = map (\x -> if length x==1 then x else (x \\ matched)) rules

part2 rules nearby myTicket =  product (map snd depart)
    where
        ticketOrder = ticketRules rules nearby
        ticket = doStep ticketOrder
        values = zip (map (\[(x,_,_)] -> x) ticket) myTicket
        depart = filter (\(x,_) -> isPrefixOf "departure" x) values


