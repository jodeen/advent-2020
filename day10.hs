import Data.List
import Data.Maybe
import Day10Data (day10Data)

sampleData1 = [
    16,
    10,
    15,
    5,
    1,
    11,
    7,
    19,
    6,
    12,
    4] :: [Int]

sampleData2 = [
    28,
    33,
    18,
    42,
    31,
    14,
    46,
    20,
    48,
    47,
    24,
    23,
    49,
    45,
    19,
    38,
    39,
    11,
    1,
    32,
    25,
    35,
    8,
    17,
    7,
    9,
    4,
    2,
    34,
    10,
    3] :: [Int]


inc3 :: Int -> (Int, Int, Int) -> (Int, Int, Int)
inc3 1 (a,b,c) = (a+1,b,c)
inc3 2 (a,b,c) = (a,b+1,c)
inc3 3 (a,b,c) = (a,b,c+1)

findPath :: [Int] -> (Int, Int, Int)
findPath [] = (0,0,0)
findPath [_] = (0,0,1)
findPath (x:y:xs)
    | x+1 == y = inc3 1 (findPath (y:xs))
    | x+2 == y = inc3 2 (findPath (y:xs))
    | x+3 == y = inc3 3 (findPath (y:xs))
    | otherwise = findPath (x : xs)

part1 d = a * c
    where 
        (a, _, c) = findPath (0 : (sort d))

validCount (a,b) = if b-a <= 3 then 1 else 0

countPath :: [Int] -> Int
countPath [] = 0
countPath [a,b] = validCount (a,b)
countPath [a,b,c] = validCount (a,b) + validCount (a,c) + countPath [b,c]
countPath (a:b:c:d:xs) = validCount (a,b) + validCount (a,c) + validCount (a,d) + countPath (b:c:d:xs)

-- sliding :: [Int] -> [[Int]]
sliding [] = []
sliding x = take 4 x : sliding (tail x)

edges :: [Int] -> [(Int, [Int])]
edges d = map edge (sliding d)
    where
         edge = \(x:xs) -> (x, filter (\y -> y <= x+3) xs)

input = (0: sort day10Data)

goal = last input

arrCounts = map arrCount input


arrCount :: Int -> (Int, Int)
arrCount x = if x == goal then (goal, 1)
  else (x, sum (map (\x -> fromJust (lookup x arrCounts)) (fromMaybe [] (lookup x (edges input)))))

part2 = lookup 0 arrCounts