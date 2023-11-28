import DayData(day2Data)


isValid :: (Integer, Integer, Char, String) -> Bool
isValid (min,max,char, s) = between min max (countChar char s)

part1 :: [(Integer, Integer, Char, String)] -> Int
part1 = length . filter isValid

between :: Integer -> Integer -> Int -> Bool
between x y c = (c >= fromIntegral x) && (c<=fromIntegral y)

countChar :: Char -> String -> Int
countChar c s = length $ filter (== c) s

sampleData = [(1,3,'a', "abcde"), (1,3,'b',"cdefg"),(2,9,'c', "ccccccccc")]

toIndex :: Integer -> Int
toIndex a = (fromIntegral a) - 1

isValid2 :: (Integer, Integer, Char, String) -> Bool
isValid2 (a,b,char, s) = (x || y) && (not (x && y))
    where 
        x = (s !! toIndex a) == char
        y = (s !! toIndex b) == char


part2 :: [(Integer, Integer, Char, String)] -> Int
part2 = length . filter isValid2