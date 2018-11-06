map'::(a -> b) -> [a] -> [b]
map' f l = [f x | x <- l]

map''::(a -> b) -> [a] -> [b]
map'' _ [] = []
map'' f (x:xs) = f x:map'' f xs

filter'::(a -> Bool) -> [a] -> [a]
filter' f l = [x | x <- l , f x]

filter''::(a -> Bool) -> [a] -> [a]
filter'' _ [] = []
filter'' f (x:xs)
    | f x = x:filter'' f xs
    | otherwise = filter'' f xs

dropWhile'::(a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' f (x:xs)
    | f x = dropWhile' f xs
    | otherwise = x:xs

partition'::(a -> Bool) -> [a] -> ([a],[a])
partition' _ [] = ([],[])
partition' f (x:xs)
    | f x = (x:i,j)
    | otherwise = (i,x:j)
    where (i,j) = partition' f xs

all'::(a -> Bool) -> [a] -> Bool
all' _ [] = True
all' f (x:xs) = f x && all' f xs
    -- | f x = all' f xs
    -- | otherwise = False

any'::(a -> Bool) -> [a] -> Bool
any' _ [] = False
any' f (x:xs) = f x || any' f xs
    -- | f x = True
    -- | otherwise = any' f xs

foldl'::(a -> b -> b) -> b -> [a] -> b
foldl' _ acc [] = acc
foldl' f acc (x:xs) = foldl' f (f x acc) xs

foldr'::(a -> b -> b) -> b -> [a] -> b
foldr' _ acc [] = acc
foldr' f acc (x:xs) = f x (foldr' f acc xs)

main::IO()
main = do
    print $ map' (+1) [1..10]
    print $ map' (reverse) [[1..5],[6..9],[5..7]]
    print $ map' (\x -> x - 1) [1..10]
    print $ map'' (+6) [1..5]
    print $ filter' (even) [1..10]
    print $ filter' (>5) [1..10]
    print $ filter' (/=3) [1,3,3,8,9,6,3,7,4,3]
    print $ filter'' (odd) [1..10]
    print $ dropWhile' (<2) [1..5]++[2]
    print $ partition' (even) [1..10]
    print $ all' (>2) [3..9]
    print $ all' (<2) [3..9]
    print $ any' (==7) [3..9]
    print $ any' (==1) [3..9]
    print $ foldl' (\x -> \y -> x - y) 0 [1..2]
    print $ foldl' (\x -> \y -> x*2 + y) 0 [1..2]
    print $ foldr' (-) 0 [1..2]
    print $ foldr' (\x -> \y -> x + y*2) 0 [1..2]
    print $ foldl' (\x -> \y -> x : y) [] [1..5]