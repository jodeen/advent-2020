import qualified Data.Map as Map
import Data.List

swap (a,b) = (b,a)


step :: (Int, Map.Map Int Int) -> Int -> (Int, Map.Map Int Int)
step (a,m) stepValue = (speak, Map.insert a (stepValue-1) m)
    where
        speak = if Map.member a m then stepValue - (Map.findWithDefault 0 a m) - 1 else 0

part1 start end = fst (foldl' step (0, init) [(2+length start) .. end])
    where
        init = Map.fromList (zip start [1..])
