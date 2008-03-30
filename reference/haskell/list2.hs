import Data.List

-- List.flatten == Data.List.concat

-- List2.mapi

-- > mapi (\i x -> i) [1,1,1]
-- [1,2,3]
mapi :: (Int -> a -> b) -> [a] -> [b]
mapi f xs = map (\(i,x) -> f i x) (zip [1..] xs)

-- List2.iteri
-- mapM (\x -> putStrLn (show x)) (mapi (\i x -> i) [1,1,1])

-- List2.index == Data.List.elemIndex
-- List2.has == List.exists = Data.List.any
-- List2.drop == Data.List.drop
-- List2.take == Data.List.take
-- List2.takedrop == Data.List.splitAt

-- List2.prefix
-- prefix (==) [1,2,3,4,5] [1,2,4,5,6]
-- [1,2]
prefix :: (a->b->Bool) -> [a] -> [b] -> [a]
prefix f [] _ = []
prefix f _ [] = []
prefix f (x:xs) (y:ys) = if f x y then (x : prefix f xs ys) else []

-- List2.diff
-- List2.diff_sorted
-- List2.segment
-- List2.segment_remove
-- List2.enumerate

-- List2.group == Data.List.group except for the equality test
-- List2.distribute

-- List2.map_equiv
-- Example: Uniquify a set of strings by adding a numerical suffix.
-- add_suffix n s = s ++ "-" ++ show (n+1)
-- f [a] = [a]
-- f lst = mapi add_suffix lst
-- map_equiv (==) f ["a","b","c","b","c","d","c","b"]
-- ["a", "b-1", "c-1", "b-2", "c-2", "d", "c-3", "b-3"]
map_equiv :: Ord a => (a->a->Ordering) -> ([a]->[b]) -> [a] -> [b]
map_equiv cmp f xs = 
    let enum = mapi (\i x -> (i, x)) xs in
    let sorted = Data.List.sortBy (\(_, a) (_, b) -> compare a b) enum in
    let groups = Data.List.groupBy (\(_, a) (_, b) -> a == b) sorted in
    let mapped_groups = map f (Data.List.map (map snd) groups) in
    let index_groups = Data.List.map (map fst) groups in
    map snd (Data.List.sortBy
	         (\a b -> compare (fst a) (fst b))
		 (Data.List.concat
	    		(map (\(a,b) -> zip a b)
			(zip index_groups mapped_groups))))
	       
-- List2.uniq_sorted
-- List2.uniq
-- List2.uniq_stable
-- List2.map_count
-- List2.make_distinct
-- List2.map3
-- List2.map4
-- List2.map5
-- List2.map6
-- List2.map7
-- List2.map8
-- List2.is_uniq
-- List2.seq
-- List2.list_init
-- List2.transpose
-- List2.extend
-- List2.intersperse
-- List2.split3
-- List2.sorted_intersection
-- List2.intersection
-- List2.filter_out
-- List2.format

