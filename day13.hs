
import Data.List 
import Data.Ord

sampleTs = 939 :: Int
sampleData = ["7", "13", "x", "x", "59", "x", "31", "19"]

day13Ts = 1002576 :: Int
day13Data = ["13","x","x","x","x","x","x","37","x","x","x","x","x","449","x","29","x","x","x","x","x","x","x","x","x","x","x","x","x","x","x","x","19","x","x","x","23","x","x","x","x","x","x","x","773","x","x","x","x","x","x","x","x","x","41","x","x","x","x","x","x","17"]



parse ::[String] -> [Int]
parse d = map read (filter (/= "x") d)

waitingTime departTime freq = freq - mod departTime freq

part1 :: Int -> [Int] -> Int
part1 ts d = busId * waiting
    where
        waitingTimes = zip d (map (waitingTime ts) d)
        (busId, waiting) = minimumBy (comparing snd) waitingTimes

checkMatch :: (String, Int) -> (Int -> Bool)
checkMatch ("x", _) = const True
checkMatch (x,offset) = (\n -> 0 == mod (n + offset) (read x::Int))

parse2 :: [String] -> [(Integer, Integer)]
parse2 s = map (\(a,b) -> (read a, (read a) + b)) (filter ((/= "x") . fst) (zip s [0,-1..]))


extendedGcd ::Integer -> Integer -> (Integer, Integer)
extendedGcd _ 0 = (1, 0)
extendedGcd a b = (t, s -q * t)
    where
        (s, t) = extendedGcd b r
        (q, r) = quotRem a b

cRemainder :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
cRemainder (n1, a1) (n2, a2) = (n1*n2, mod (a1*m2*n2 + a2*m1*n1) (n1*n2))
    where 
        (m1,m2) = extendedGcd n1 n2

part2 :: [(Integer,Integer)] -> Integer
part2 d = snd (foldl1 cRemainder d)