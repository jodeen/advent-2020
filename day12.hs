import Day12Data(day12Data)

data State = State {facing :: Int, x :: Int, y:: Int} deriving(Show)
type Move = (Char, Int)
data State2 = State2 {waypoint::(Int,Int), ship::(Int, Int)} deriving(Show)

sampleData = [
    ('F',10),
    ('N',3),
    ('F',7),
    ('R',90),
    ('F',11)] :: [Move]

incX :: State -> Int -> State
incX state dist = state {x= (x state) + dist}

incY :: State -> Int -> State
incY state dist = state {y= (y state) + dist}

move :: State -> Move -> State
move s ('N', dist) = incY s dist
move s ('S', dist) = incY s (-1 * dist)
move s ('E', dist) = incX s dist
move s ('W', dist) = incX s (-1 * dist)
move s ('F', dist) = case facing s of
     0 -> move s ('E', dist)
     90 -> move s ('S', dist)
     180 -> move s ('W', dist)
     270 -> move s ('N', dist)
move s ('R', dist) = s {facing = (mod (facing s + dist) 360 )}
move s ('L', dist) = s {facing = (mod (facing s - dist + 360) 360)}

part1 moves = abs (x finalState) + abs (y finalState)
    where 
        finalState = foldl move (State 0 0 0)  moves


inc (x,y) (a,b) = (x+a,y+b)
incMult (x,y) (a,b) m = (x+m*a,y+m*b)

rshift (x,y) = (y,-1 * x)
lshift (x,y) = (-1*y,x)
around = rshift . rshift

move2 :: State2 -> Move -> State2
move2 s ('N', dist) = s {waypoint = inc (waypoint s) (0,dist)}
move2 s ('S', dist) = s {waypoint = inc (waypoint s) (0,-1*dist)}
move2 s ('E', dist) = s {waypoint = inc (waypoint s) (dist,0)}
move2 s ('W', dist) = s {waypoint = inc (waypoint s) (-1 *dist,0)}
move2 s ('F', dist) = s {ship = incMult (ship s) (waypoint s) dist}
move2 s ('R', dist) = case dist of 
    90 -> s {waypoint = rshift (waypoint s)}
    180 -> s {waypoint = around (waypoint s)}
    270 -> s {waypoint = lshift (waypoint s)}
move2 s ('L', dist) = case dist of 
    90 -> s {waypoint = lshift (waypoint s)}
    180 -> s {waypoint = around (waypoint s)}
    270 -> s {waypoint = rshift (waypoint s)}


part2 moves = abs x + abs y
    where 
        (x,y) = ship (foldl move2 (State2 (10,1) (0,0))  moves)