hanoi::Int -> Char -> Char -> Char -> [(Char,Char)]
hanoi 1 a _ c = [(a,c)]
hanoi n a b c = (hanoi (n-1) a c b) ++ [(a,c)] ++ (hanoi (n-1) b a c)

main = do
    print $ hanoi 3 'A' 'B' 'C'
    