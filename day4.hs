import DayData(day4Data)
import Data.List
import Data.Maybe

d = "iyr:2013 hcl:#ceb3a1\
\hgt:151cm eyr:2030\
\byr:1943 ecl:grn\
\"

sampleData = "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd byr:1937 iyr:2017 cid:147 hgt:183cm"

toPair :: String -> (String, String)
toPair t = (take 3 t, drop 4 t)

toPairs :: String -> [(String,String)]
toPairs s = map toPair (words s)

containsAll :: Eq a => [a] -> [a] -> Bool
containsAll a c = length (intersect a c) == length c

isValid :: [(String,String)] -> Bool
isValid pairs = containsAll (map fst pairs) ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

part1 s = length (filter isValid (map toPairs s))

allIn :: String -> String -> Bool
allIn valid candidate = isNothing (find (\e -> notElem e valid) candidate)
yearCheck min max y = (length y == 4) && (y >= min) && (y <= max)

isPairValid :: (String,String) -> Bool
isPairValid ("byr", y) = yearCheck "1920" "2002" y
isPairValid ("iyr", y) = yearCheck "2010" "2020" y
isPairValid ("eyr", y) = yearCheck "2020" "2030" y
isPairValid ("hgt", [a,b,c,'c','m']) = [a,b,c] >= "150" && [a,b,c] <= "193"
isPairValid ("hgt", [a,b,'i','n']) = [a,b] >= "59" && [a,b] <= "76"
isPairValid ("hgt", _) = False
isPairValid ("hcl", '#':n) = (length n == 6) && (allIn (['0'..'9'] ++ ['a'..'f']) n)
isPairValid ("hcl", _) = False
isPairValid ("ecl", c) = elem c ["amb","blu","brn","gry","grn","hzl","oth"]
isPairValid ("pid", p) = (length p == 9) && (allIn ['0'..'9'] p)
isPairValid ("cid", _) = True
isPairValid _ = True

isValid2 :: [(String,String)] -> Bool
isValid2 pairs = (isValid pairs) && (isNothing (find (not . isPairValid) pairs)) 

invalidSample = [
    "eyr:1972 cid:100 hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926",
    "iyr:2019 hcl:#602927 eyr:1967 hgt:170cm ecl:grn pid:012533040 byr:1946",
    "hcl:dab227 iyr:2012 ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277",
    "hgt:59cm ecl:zzz eyr:2038 hcl:74454a iyr:2023 pid:3556412378 byr:2007"]

part2 s = length (filter isValid2 (map toPairs s))