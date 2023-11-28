import Data.List
import Day18Data

sampleData = "2 * 3 + (4 * 5)"

type State = (Int, Int -> Int)

parse :: State -> Char -> State
parse s ' ' = s
parse (total, fn) '+' = (total, (total +))
parse (total, fn) '*' = (total, (total *))
parse (_, fn) c = (fn (read [c]), fn)

parseR :: (State, String) -> (State, String)
parseR (s, []) = (s, [])
parseR (s, ')' : xs) = (s, xs)
parseR ((total, fn), '(' : xs) = parseR ((fn nested, fn), rem)
  where
    ((nested, _), rem) = parseR ((0, id), xs)
parseR (s, c : xs) = parseR ((parse s c), xs)

doParse s = fst (foldl parse (0, id) s)

doParseR s = total
  where
    ((total, _), _) = parseR ((0, id), s)

part1 i = sum (map doParseR i)

-- greaterPrecedence :: Char -> Char -> Bool
-- greaterPrecedence '+' '+' = False
-- greaterPrecedence '+' _ = True
-- greaterPrecedence '*' '*' = False
-- greaterPrecedence '*' '*' = False

data Token = Digit Int | Plus | Mult | RParen | LParen | Empty deriving (Show, Eq)

type YardState = ([Token], [Token], String, [Int])

parseToken :: Char -> Token
parseToken x | x `elem` "0123456789" = Digit (read [x])
parseToken '+' = Plus
parseToken '*' = Mult
parseToken '(' = LParen
parseToken ')' = RParen
parseToken _ = Empty

tokenize = map parseToken

applyOp :: Token -> [Int] -> Int
applyOp Plus (a : b : _) = a + b
applyOp Mult (a : b : _) = a * b
applyOp _ _ = 0

shuntingYard :: YardState -> YardState
shuntingYard ([], [], rpn, res) = ([], [], rpn, res)
shuntingYard ([], o : ss, rpn, res) = ([], ss, show o ++ " " ++ rpn, (applyOp o res) : (drop 2 res))
shuntingYard (Empty : xs, stack, rpn, res) = (xs, stack, rpn, res)
shuntingYard (Plus : xs, [], rpn, res) = (xs, [Plus], rpn, res)
shuntingYard (Mult : xs, [], rpn, res) = (xs, [Mult], rpn, res)
shuntingYard (Plus : xs, Plus : ss, rpn, res) = (xs, Plus : Plus : ss, rpn, res)
shuntingYard (Plus : xs, Mult : ss, rpn, res) = (Plus : xs, ss, show Mult ++ " " ++ rpn, (applyOp Mult res) : (drop 2 res))
shuntingYard (Plus : xs, LParen : ss, rpn, res) = (xs, Plus : LParen : ss, rpn, res)
shuntingYard (Mult : xs, Mult : ss, rpn, res) = (xs, Mult : Mult : ss, rpn, res)
shuntingYard (Mult : xs, Plus : ss, rpn, res) = (xs, Mult : Plus : ss, rpn, res)
shuntingYard (Mult : xs, LParen : ss, rpn, res) = (xs, Mult : LParen : ss, rpn, res)
shuntingYard (LParen : xs, ss, rpn, res) = (xs, LParen : ss, rpn, res)
shuntingYard (RParen : xs, LParen : ss, rpn, res) = (xs, ss, rpn, res)
shuntingYard (RParen : xs, o : ss, rpn, res) = (RParen : xs, ss, show o ++ " " ++ rpn, (applyOp o res) : (drop 2 res))
shuntingYard ((Digit x) : xs, ss, rpn, res) = (xs, ss, (show x) ++ " " ++ rpn, x : res)
shuntingYard a = ([], [], [], [])

isNotDone :: YardState -> Bool
isNotDone ([], [], _, _) = False
isNotDone _ = True

toRPN :: String -> YardState
toRPN input = head (dropWhile isNotDone (iterate shuntingYard (tokenize input, [], "", [])))