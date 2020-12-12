{-# LANGUAGE LambdaCase #-}

import Day11Data(day11Data)
import Data.List

import qualified Data.Map as Map

data Seat = Empty | Floor | Occupied deriving (Show, Eq)
type Spot = ((Int, Int), Seat)

type Layout = Map.Map (Int, Int) Seat


sampleData = [
    "L.LL.LL.LL",
    "LLLLLLL.LL",
    "L.L.L..L..",
    "LLLL.LL.LL",
    "L.LL.LL.LL",
    "L.LLLLL.LL",
    "..L.L.....",
    "LLLLLLLLLL",
    "L.LLLLLL.L",
    "L.LLLLL.LL"]

sample2Data = [
    ".............",
    ".L.L.#.#.#.#.",
    "............."]

sample3Data = [
    ".......#.",
    "...#.....",
    ".#.......",
    ".........",
    "..#L....#",
    "....#....",
    ".........",
    "#........",
    "...#....."
    ]

parse :: [String] -> Layout
parse d = Map.fromList spots
    where
        spots = concatMap (\y -> parseRow y (d !! y)) [0..length d - 1]

parseRow :: Int -> String -> [Spot]
parseRow i d = map (\x -> ((x, i), parseSeat (d !! x))) [0..length d - 1]

parseSeat :: Char -> Seat
parseSeat 'L' = Empty
parseSeat '.' = Floor
parseSeat '#' = Occupied

slice idx num list = take (num +  (min idx 0)) (drop idx list)

adjacents :: (Int, Int) -> [(Int,Int)]
adjacents (x,y) = filter (/= (x,y)) [(x',y') | x'<-[x-1..x+1], y'<-[y-1..y+1]]
    
isOccupied :: Maybe Seat -> Bool
isOccupied (Just Occupied) = True
isOccupied _ = False

countOccupied :: Layout -> Int
countOccupied layout = sum (map (\x -> if x == Occupied then 1 else 0) (Map.elems layout))


countAdjacents :: Layout -> (Int,Int) -> Int
countAdjacents layout point = length (filter isOccupied (map (`Map.lookup` layout) (adjacents point)))


doStep :: Layout -> (Int,Int) -> Seat -> Seat 
doStep d (x,y) Empty = if countAdjacents d (x, y) == 0 then Occupied else Empty
doStep d (x,y) Occupied = if countAdjacents d (x, y) >= 4 then Empty else Occupied
doStep _ _ s = s

doRound :: Layout -> Layout
doRound layout = Map.mapWithKey (doStep layout) layout


-- doRound :: [[Spot]] -> [[Spot]]
-- doRound d = map (map (doStep d)) d

-- printSpot :: Spot -> Char
-- printSpot (Empty, _, _) = 'L'
-- printSpot (Occupied, _, _) = '#'
-- printSpot (Floor, _, _) = '.'

-- doPrint d = mapM print (map (map printSpot) d)

findStable :: Layout -> Layout
findStable d = fst (head (dropWhile (\(a,b) -> a /= b) (zip allRounds (tail allRounds))))
    where
        allRounds = iterate doRound d

part1 :: Layout -> Int
part1 d = countOccupied (findStable d)

isOccupiedLine :: Layout -> [(Int,Int)] -> Bool
isOccupiedLine layout points = isOccupied (head (dropWhile continueSearch (map (`Map.lookup` layout) points)))
    where
        continueSearch = \case
            Just Floor -> True
            _ -> False


countAdjacents2 :: Layout -> (Int,Int) -> Int
countAdjacents2 layout (x,y) = length (filter id (map (isOccupiedLine layout) [n,e,s,w,ne,se,sw,nw]))
    where
        n = [(x,y-n) | n <- [1..]]
        e = [(x+n,y) | n <- [1..]]
        s = [(x,y+n) | n <- [1..]]
        w = [(x-n,y) | n <- [1..]]
        ne = [(x+n,y-n) | n <- [1..]]
        se = [(x+n,y+n) | n <- [1..]]
        sw = [(x-n,y+n) | n <- [1..]]
        nw = [(x-n,y-n) | n <- [1..]]


doStep2 :: Layout -> (Int,Int) -> Seat -> Seat 
doStep2 d (x,y) Empty = if countAdjacents2 d (x, y) == 0 then Occupied else Empty
doStep2 d (x,y) Occupied = if countAdjacents2 d (x, y) >= 5 then Empty else Occupied
doStep2 _ _ s = s        

doRound2 :: Layout -> Layout
doRound2 layout = Map.mapWithKey (doStep2 layout) layout


findStable2 :: Layout -> Layout
findStable2 d = fst (head (dropWhile (\(a,b) -> a /= b) (zip allRounds (tail allRounds))))
    where
        allRounds = iterate doRound2 d

part2 :: Layout -> Int
part2 d = countOccupied (findStable2 d)        