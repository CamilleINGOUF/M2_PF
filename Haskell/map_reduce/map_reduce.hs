import Data.List

map_reduce_1 :: Ord t1 => (t2 -> [a]) -> (a -> [(t1, b)]) -> ([b] -> t) -> t2 -> [(t1, t)]
map_reduce_1 the_split the_map the_reduce dat = map (\(a,b) -> (a,(the_reduce b))) (shuffle (concat (map the_map (the_split dat))))

wc_split::String -> [String]
wc_split dat = cut '\n' dat

cut::Char -> String -> [String]
cut _ "" = [""]
cut sep (h:t)
            | h == sep = "":x:rest
            | otherwise = (h:x):rest
            where (x:rest) = cut sep t

wc_map::[Char] -> [(String, Int)]
wc_map l = map (\x -> (x,1)) (cut ' ' l)

join' :: Ord a => [(a,b)] -> [(a,[b])]
join' [] = []
join' [(k,v)] = [(k,[v])]
join' ((x1,x2):xs)
    | a == x1 = (a,x2:b):t
    | otherwise = (x1,[x2]):(a,b):t
    where ((a,b):t) = join' xs

shuffle :: Ord a => [(a,b)] -> [(a,[b])]
shuffle l = join' (sortBy (\(a,_) (b,_) -> compare a b) l)

wc_reduce :: [Int] -> Int
wc_reduce = foldl (+) 0

main::IO()
main =
    let dat = "la la la la\nvoila le temps\ndes cerises\ndes bonnes cerises\ndes oiseaux sont la aussi"        
    in do
        print $ map_reduce_1 wc_split wc_map wc_reduce dat