length'::(Floating a) => [(a,a)] -> a
length' [x] = 0
length' (x:y:xs) = sqrt ((fst y - fst x)^2 + (snd y - snd x)^2) + length' (y:xs)

main = do
    print $ length' [(1,2),(8,7),(6,6)]