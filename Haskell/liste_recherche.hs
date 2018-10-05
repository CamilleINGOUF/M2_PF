search::(Eq a) => [a] -> a -> Bool
search [] _ = False
search (x:xs) v
    | x == v = True
    | otherwise = search xs v

searchValue::(Eq a) => [(a,b)] -> a -> b
searchValue (x:xs) k
    | k == (fst x) = (snd x)
    | otherwise = (searchValue xs k)

main = do
    print $ search [1,2,3,4] 9
    print $ searchValue [(1,'a'),(2,'b'),(3,'c')] 3