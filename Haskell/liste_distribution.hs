halfDeckT::[a] -> [a] -> [[a]]
halfDeckT [] _ = []
halfDeckT (x:xs) l
    | length xs == length l = [ xs , x:l ]
    | length (x:xs) == length l = [ x:xs , l ]
    | otherwise = halfDeckT xs (x:l)

main = do
    print $ halfDeckT [1,2,3,4,5,6,7] []