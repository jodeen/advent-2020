import Day8Data(day8Data)
import Data.List
import Data.Maybe

data InstType = NOP | ACC | JMP deriving(Show)
type Instruction = (InstType, Int)
data State = State {
    insts :: [Instruction],
    addrs :: [Int],
    acc :: Int
 } deriving(Show)

sampleData = [
    "nop +0",
    "acc +1",
    "jmp +4",
    "acc +3",
    "jmp -3",
    "acc -99",
    "acc +1",
    "jmp -4",
    "acc +6"]

sampleData2 = [
    "nop +0",
    "acc +1",
    "jmp +4",
    "acc +3",
    "jmp -3",
    "acc -99",
    "acc +1",
    "nop -4",
    "acc +6"]    

toInst :: [String] -> [Instruction]
toInst = map (parseInst .words)

parseInst :: [String] -> Instruction
parseInst ["acc", x] = (ACC, parseArg x)
parseInst ["jmp",x] = (JMP, parseArg x)
parseInst ["nop",x] = (NOP, parseArg x)


parseArg :: String -> Int
parseArg ('+':i) = read i 
parseArg i = read i

nextAddr :: State -> [Int]
nextAddr State {addrs = a} = 1 + head a : a

doInst :: State -> Instruction -> State
doInst state (ACC, value) = state {acc = value + acc state, addrs = nextAddr state}
doInst state (JMP, value) = state {addrs = (value + head (addrs state)) : addrs state}
doInst state (NOP, _) = state {addrs = nextAddr state}

isLoop :: State -> Bool
isLoop State{addrs=a} = nub a == a


processState :: State -> State
processState state = doInst state inst
    where 
        inst = insts state !! head (addrs state)

willNextLoop :: State -> Bool
willNextLoop state = length (nub (addrs nextState)) == length (addrs state)
    where
        nextState = processState state

findLoop :: [Instruction] -> State
findLoop insts = head (dropWhile (not . willNextLoop) (iterate processState (State insts [0] 0)))

isTerm :: State -> Bool
isTerm State{addrs=(a:_), insts=i} = a == length i

part1 = acc (findLoop (toInst day8Data))

doesFinish :: [Instruction] -> Maybe State
doesFinish insts = if isTerm l then Just l else Nothing
    where
        l = head (dropWhile (\s -> not (isTerm s) && not (willNextLoop s))  (iterate processState (State insts [0] 0)))

replace :: [a] -> Int -> (a -> a) -> [a]
replace list idx fn = a ++ (fn b) : c
    where
        (a, b : c) = splitAt idx list

doRewrite :: [Instruction] -> Int -> [Instruction]
doRewrite insts idx  = replace insts idx rewrite 
    where
        rewrite = \x -> case x of
            (JMP, x) -> (NOP, x)
            (NOP, x) -> (JMP, x)
            x -> x

allRewrite insts = map (doRewrite insts) [0..(length insts)]

part2 insts = acc final
    where
        Just final = head (filter isJust (map doesFinish (allRewrite insts)))
