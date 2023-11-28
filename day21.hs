
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List
import Day21Data

sampleData = [
    (["mxmxvkd", "kfcds", "sqjhc", "nhms"], ["dairy", "fish"]),
    (["trh", "fvjkl", "sbzzf", "mxmxvkd"], ["dairy"]),
    (["sqjhc", "fvjkl"], ["soy"]),
    (["sqjhc", "mxmxvkd", "sbzzf"], ["fish"])]

toKV l = concat (map (\(ing, allerg) -> (map (\s-> (s,ing)) allerg)) l )


-- upsert :: Maybe [Int] -> Maybe [Int]

toMap l = foldl' (\m (k,v) -> Map.alter (upsert v) k m)  Map.empty  l
    where
        upsert new orig = case orig of
            Just l -> Just (intersect l new)
            Nothing -> Just new

part1 :: [([String],[String])] -> Int
part1 d = length (filter (\x -> notElem x possibleAllergens) allIng)
    where
        map = toMap (toKV d)
        possibleAllergens = nub (concat (Map.elems map))
        allIng = concatMap fst d