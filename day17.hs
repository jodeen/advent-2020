
import Data.List
import qualified Data.Map as Map

sampleData = [
    ".#.",
    "..#",
    "###"]

day17Data = [
    "...#..#.",
    "..##.##.",
    "..#.....",
    "....#...",
    "#.##...#",
    "####..##",
    "...##.#.",
    "#.#.#..."]

type Point4 = (Int, Int, Int, Int)

neighbors (x,y,z,w) = [(x',y',z',w') | x' <- [x-1..x+1], y' <- [y-1..y+1], z' <- [z-1..z+1], w' <- [w-1..w+1]] \\ [(x,y,z,w)]

initState :: [String] -> Map.Map Point4 Bool 
initState strings = Map.fromList (concat (zipWith row strings  [0..]))
    where 
        row string y  = zipWith (\c x-> ((x,y,0,0), c == '#')) string [0..] 

countActive state p = length (filter id (map mFind (neighbors p) ))
    where
        mFind e = Map.findWithDefault False e state

newState :: Map.Map Point4 Bool -> Point4 -> (Point4, Bool)
newState state point  = if current then (point, (active == 2) || (active == 3)) else (point, active == 3)
    where
        active = countActive state point
        current = Map.findWithDefault False point state

doStep initState = Map.fromList (map (newState initState) full)
    where 
        (minX, minY, minZ,minW) = foldl1' (\(x,y,z,w) (x', y', z', w') -> (min x x', min y y', min z z', min w w')) (Map.keys initState)
        (maxX, maxY, maxZ,maxW) = foldl1' (\(x,y,z,w) (x', y', z', w') -> (max x x', max y y', max z z', max w w')) (Map.keys initState)
        full = [(x,y,z,0) | x<-[minX-1..maxX+1], y<-[minY-1..maxY+1], z<-[minZ-1..maxZ+1]]

doStep2 initState = Map.fromList (map (newState initState) full)
    where 
        (minX, minY, minZ,minW) = foldl1' (\(x,y,z,w) (x', y', z', w') -> (min x x', min y y', min z z', min w w')) (Map.keys initState)
        (maxX, maxY, maxZ,maxW) = foldl1' (\(x,y,z,w) (x', y', z', w') -> (max x x', max y y', max z z', max w w')) (Map.keys initState)
        full = [(x,y,z,w) | x<-[minX-1..maxX+1], y<-[minY-1..maxY+1], z<-[minZ-1..maxZ+1], w<-[minW-1..maxW+1]]

countAllActive state =  length (filter id (Map.elems state))

part1 :: Map.Map Point4 Bool -> Int
part1 init = countAllActive (last (take 7 (iterate doStep init)))

part2 :: Map.Map Point4 Bool -> Int
part2 init = countAllActive (last (take 7 (iterate doStep2 init)))