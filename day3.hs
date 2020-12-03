import DayData(day3Data)
import Data.List

len = 31

isTree :: Int -> (String, Int) -> Bool
isTree right (s, i) = '#' == (head (drop (mod (i*right) (length s)) s))

countTrees :: Int -> [String] -> Int
countTrees right m = length (filter (isTree right) (zip m [0..]))

part1 = countTrees 3

skip :: [a] -> [a]
skip a = map fst (filter (\(_,x) -> 0 == (mod x 2)) (zip a [0..]))

part2 s = product (map (\f -> f s) ((countTrees 1 . skip) : (map countTrees [1,3,5,7])))

sampleData = [
    "..##.......",
    "#...#...#..",
    ".#....#..#.",
    "..#.#...#.#",
    ".#...##..#.",
    "..#.##.....",
    ".#.#.#....#",
    ".#........#",
    "#.##...#...",
    "#...##....#",
    ".#..#...#.#"]