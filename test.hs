import qualified Data.Map as Map
import qualified Data.Set as Set


fromList' :: (Ord k) => [(k,v)] -> Map.Map k v  
fromList' = foldr (\(k,v) acc -> Map.insert k v acc) Map.empty



double x = x + x

isLessThanTen x = if x < 10
	then True
	else False

test xs = sum [1 | x <- xs]

