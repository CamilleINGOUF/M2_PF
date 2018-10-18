length'::(Floating a) => [(a,a)] -> a
length' [] = 0
length' [_] = 0
length' (x:y:xs) = (len x y) + length' (y:xs)

len:: (Floating a) => (a, a) -> (a, a) -> a
len x y = sqrt ((fst y - fst x)^2 + (snd y - snd x)^2)

main = do
    print $ length' [(0,0),(3,4),(13,4)]