maximums:: (Ord a) => [a] -> a -> a
maximums [] n = n
maximums (x:xs) n
    | n > x = maximums xs n
    | otherwise = maximums xs x

maxListe:: (Ord a) => [a] -> a
maxListe (x:xs) = maximums xs x

maxListes:: (Ord a) => [[a]] -> [a]
maxListes [] = []
maxListes (x:xs) = (maxListe x) : (maxListes xs)

main = do
    print $ maxListe [3,8,9,67,100,56]
    print $ maxListes [[3,8,9,67,100,56],[3,8,9,67,100,897],[3,8,109,67,100,56]]