size::[a] -> Int
size [] = 0
size (x:xs) = 1 + size xs

sum'::(Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

double::(Num a) => [a] -> [a]
double [] = []
double (x:xs) = (x*2) : (double xs)

evens::(Integral a) => [a] -> [a]
evens [] = []
evens (x:xs)
    | even x = x : evens xs
    | otherwise = evens xs

moreThan::(Ord a) => [a] -> a -> [a]
moreThan [] _ = []
moreThan (x:xs) n
    | x > n = x : moreThan xs n
    | otherwise = moreThan xs n

concatWords::[String] -> String
concatWords [] = ""
concatWords (x:xs) = x ++ concatWords xs

concatListe::[a] -> [a] -> [a]
concatListe [] l = l
concatListe (x:xs) l = (x : (concatListe xs l))

add::[a] -> a -> Int -> [a]
add [] v _ = [v]
add l v 0 = v : l
add (x:xs) v n = x : add xs v (n-1)

delete::[a] -> Int -> [a]
delete [] _ = []
delete (x:xs) 0 = xs
delete (x:xs) n = x : (delete xs (n-1))

main = do
    print $ size [1,2,3]
    print $ sum' [1,2,3.5]
    print $ double [1,5,3]
    print $ evens [1,2,3,4,6,7,8,9,0]
    print $ moreThan [1,2,3,4,6,7,8,9,0] 4
    print $ concatWords ["oui","non","peut","etre"]
    print $ concatListe [1,5,3] [1,2,3,4,6,7,8,9,0]
    print $ add [1,2,3,5] 4 3 
    print $ delete [1,2,3,5] 2
