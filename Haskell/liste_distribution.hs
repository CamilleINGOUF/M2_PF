halfDeckT::[a] -> [a] -> [[a]]
halfDeckT [] _ = []
halfDeckT (x:xs) l
    | length xs == length l = [ xs , x:l ]
    | length (x:xs) == length l = [ x:xs , l ]
    | otherwise = halfDeckT xs (x:l)

halfDeckTT::[a] -> [a] -> [a]-> ([a],[a])
halfDeckTT [] i j = (i,j)
halfDeckTT (x:xs) i j = halfDeckTT xs j (x:i)

halfDeck::[a] -> ([a],[a])
halfDeck [] = ([],[])
halfDeck (x:xs) = (x:j,i)
    where (i,j) = halfDeck xs

main = do
    print $ halfDeckT [1,2,3,4,5,6,7] []
    print $ halfDeckTT [1,2,3,4,5,6,7] [] []
    print $ halfDeck [1,2,3,4,5,6]