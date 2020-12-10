import Day9Data(day9Data)

sampleData = [35,
            20,
            15,
            25,
            47,
            40,
            62,
            55,
            65,
            95,
            102,
            117,
            150,
            182,
            127,
            219,
            299,
            277,
            309,
            576
            ] :: [Int]

isValid :: [Int] -> Bool
isValid nums = candidate `elem` sums
    where
        (candidate:preamble) = reverse nums
        sums = map (uncurry (+)) (allPairs preamble)

doPair :: Int -> [Int] -> [(Int,Int)]
doPair a nums = zip nums (repeat a)

allPairs :: [Int] -> [(Int, Int)]
allPairs [] = []
allPairs (x:xs) = doPair x xs ++ allPairs xs

windowed :: Int -> [Int] -> [[Int]]
windowed _ [] = []
windowed size nums = if length nums > size then (take size nums) : (windowed size (tail nums)) else [nums]

findInvalid size nums = last (head (filter (not . isValid) (windowed (size+1) nums)))

part1 = findInvalid 25 day9Data

allWindows :: Int -> [Int] -> [[Int]]
allWindows size nums = concat (map (\i -> windowed i nums) [1..size])


part2 = (minimum cont) + (maximum cont)
    where
        cont = head (filter (\r -> 57195069 == sum r) (allWindows 549 (take 548 day9Data)))