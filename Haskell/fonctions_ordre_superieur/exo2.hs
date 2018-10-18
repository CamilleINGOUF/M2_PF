add a = \x -> a + x

main::IO()
main = do
    print $ (\x -> \y -> x + y) 4 5
    print $ (*2) 4
    print $ (add) 5 6
    print $ map (add (add 5 8)) [1..6]
    print $ (add (add 7 8) 9)