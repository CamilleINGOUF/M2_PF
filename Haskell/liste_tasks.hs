addTask::(Int,b) -> [(Int,b)] -> [(Int,b)]
addTask n [] = [n]
addTask n (x:xs)
    | fst n <= fst x = x : (addTask n xs)
    | otherwise = n:x:xs

main = do
    let l = addTask (4,"oui") [(0,"non")]
    let ll = addTask (2,"maybe") l
    print $ addTask (4,"yay") ll