import Day6Data(day6Data)
import Data.List

-- union would probabably have done the same thing
uniqueAnswers :: [String] -> String
uniqueAnswers g = nub (intercalate "" g)

part1 d = sum (map (length . uniqueAnswers) d)

commonAnswers :: [String] -> String
commonAnswers = foldl1 intersect

part2 d = sum (map (length . commonAnswers) d)