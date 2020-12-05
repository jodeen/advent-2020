
import DayData(day5Data)
import Data.List


step :: (Int, Int) -> Char ->  (Int, Int)
step (s,e) 'F' = (s, s + ((e-s) `div` 2))
step (s,e) 'B' = (1 + s + (e-s) `div` 2, e)
step (s,e) 'L' = (s, s + ((e-s) `div` 2))
step (s,e) 'R' = (1 + s + (e-s) `div` 2, e)



row:: String -> Int
row s = fst (foldl  step (0, 127) s)

column:: String -> Int
column s = fst (foldl  step (0, 7) s)

seat :: String -> Int
seat s = (row r) * 8 + (column c)
    where 
        (r,c) = splitAt 7 s

part1 d = maximum $ map seat d

part2 d = take 5 ([(minimum $ map seat d)..] \\ map seat d)