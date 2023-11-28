import Data.List
import Day14Data
import qualified Data.Map as Map
import Data.Maybe

sampleMask = parseMask "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"

sampleData = [
    Mask "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X",
    Mem 8 11,
    Mem 7 101,
    Mem 8 0] :: [Inst]


parseMask :: String -> [Maybe Bool]
parseMask [] = []
parseMask ('X':xs) = Nothing : parseMask xs
parseMask ('0':xs) = Just False : parseMask xs
parseMask ('1':xs) = Just True : parseMask xs


toBinary :: Integer -> [Bool]
toBinary x = reverse (take 36 (unfoldr toBinary' x))
    where
        toBinary' n = Just ((1 == (mod n 2)), div n 2)

doMask :: [Maybe Bool] -> [Bool] -> [Bool]
doMask mask num = zipWith doMask' mask num 
    where 
        doMask' Nothing a = a
        doMask' (Just m) _ = m

toMaskInt :: [Maybe Bool] -> Integer -> Integer
toMaskInt mask num = toInt (doMask mask (toBinary num))

toString :: [Bool] -> String
toString [] = []
toString (True:xs) = '1' : toString xs
toString (False:xs) = '0' : toString xs

toInt :: [Bool] -> Integer
toInt bits = foldl (\last curr -> last * 2 + if curr then 1 else 0) 0 bits


-- process' ::  ([Maybe Bool], [(Int,Int)]) -> Inst ->([Maybe Bool], [(Int,Int)])


process :: [Inst] -> [(Integer, Integer)]
process insts = snd (foldl' process' (parseMask "0", [])  insts)
    where
        process' (_,list) (Mask m) = (parseMask m, list)
        process' (mask,list) (Mem m v) = (mask, (m, toMaskInt mask v):list)

part1 insts = sum (Map.elems (Map.fromList (reverse (process insts))))

generateAddrs :: [Maybe Bool] -> [[Maybe Bool]]
generateAddrs [] = [[]]
generateAddrs (Nothing:xs) = (map (Just True:) rest) ++ (map (Just False:) rest)
    where
        rest = generateAddrs xs
generateAddrs (x:xs) = map (x:) (generateAddrs xs)

maskAddrsToInt :: [Maybe Bool] -> Integer
maskAddrsToInt m = toInt (map fromJust m)

doMask2 :: [Maybe Bool] -> Integer -> [Maybe Bool]
doMask2 mask num = zipWith doMask' mask mask2 
    where 
        doMask' Nothing a = Nothing
        doMask' (Just False) a = Just a
        doMask' (Just True) _ = Just True
        mask2 = toBinary num

applyMasks :: [Maybe Bool] -> Integer -> [Integer]
applyMasks mask value = map maskAddrsToInt (generateAddrs (doMask2 mask value))

doProcess2 ::([Maybe Bool], Map.Map Integer Integer) -> Inst -> ([Maybe Bool], Map.Map Integer Integer)
doProcess2 (_, mem) (Mask mask)  = (parseMask mask, mem)
doProcess2 (mask, mem) (Mem loc value) = (mask, Map.union (Map.fromList (map (\x->(x,value)) locs)) mem)
    where 
        locs = applyMasks mask loc

sampleData2 = [
    Mask "000000000000000000000000000000X1001X",
    Mem 42 100,
    Mask "00000000000000000000000000000000X0XX",
    Mem 26 1
    ]

doPart2 insts = sum (Map.elems mem)
    where 
        (_, mem) = foldl' doProcess2 ([], Map.empty) insts