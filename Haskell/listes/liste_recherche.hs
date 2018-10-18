data Result a = Found a | NotFound

instance Show a => Show (Result a) where
    show (Found a) = show a
    show (NotFound) = "[Not Found]"

search::(Eq a) => [a] -> a -> Result a
search [] _ = NotFound
search (x:xs) v
    | x == v = Found x
    | otherwise = search xs v

searchValue::(Eq a) => [(a,b)] -> a -> Result b
searchValue [] _ = NotFound
searchValue (x:xs) k
    | k == (fst x) = Found (snd x)
    | otherwise = (searchValue xs k)

main = do
    print $ search [1,2,3,4] 5
    print $ searchValue [(1,'a'),(2,'b'),(3,'c')] 3