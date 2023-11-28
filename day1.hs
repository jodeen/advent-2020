
import DayData (day1Data)

-- yes this doen't account for the case when one of the list items is 1010
doPart1 :: [Integer] -> Integer
doPart1 l = uncurry (*) (head $ (filter (\(x,y) -> x+y  == 2020) [(x,y) | x<-l, y<-l]))
part1 = doPart1 day1Data


doPart2 :: [Integer] -> Integer
doPart2 l = product (head $ (filter (\x -> sum x == 2020) [[x,y,z] | x<-l, y<-l, z<-l]))
part2 = doPart2 day1Data