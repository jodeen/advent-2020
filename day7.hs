import Day7Data(day7Data)
import Data.List

type State = [(String, [(Int, String)])]

sampleData = [
    ("light red", [(1, "bright white"), (2, "muted yellow")]),
    ("dark orange", [(3, "bright white"), (4, "muted yellow")]),
    ("bright white", [(1, "shiny gold")]),
    ("muted yellow", [(2, "shiny gold"), (9, "faded blue")]),
    ("shiny gold", [(1, "dark olive"), (2, "vibrant plum")]),
    ("dark olive", [(3, "faded blue"), (4, "dotted black")]),
    ("vibrant plum", [(5, "faded blue"), (6, "dotted black")]),
    ("faded blue", []),
    ("dotted black", [])
    ] :: State

canBeHeldInDirect :: State -> String -> [String]
canBeHeldInDirect state color = map fst (filter (\e -> inChild (snd e) color) state)

inChild :: [(Int, String)] -> String -> Bool
inChild l color = elem color (map snd l) 

doStep :: State -> [String] -> [String]
doStep state init = nub (concatMap (canBeHeldInDirect state) init)

step :: State -> [String] -> [String]
step state init = if new == init then init else (init ++ (step state new))
    where
        new = nub (concatMap (canBeHeldInDirect state) init)

part1 :: State -> String -> Int
part1 state color = length (nub (delete color (step state [color])))

countChild :: State -> String -> Int
countChild state color = sum (map (\(num,c) -> num * (1 + countChild state c)) contents)
    where
        Just contents =lookup color state 

part2 :: State -> Int
part2 state = countChild state "shiny gold"