occ::Eq a => [a] -> a -> Integer
occ [] _ = 0
occ (x:xs) v
    | v == x = res + 1
    | otherwise = res
    where res = (occ xs v)

occT::Eq a => [a] -> a -> Integer -> Integer
occT [] _ acc = acc
occT (x:xs) v acc
    | x == v = occT xs v (acc+1)
    | otherwise = occT xs v acc

occf::(Num a, Eq b, Foldable c) => c b -> b -> a
occf l v = foldl (\acc x -> if x == v then acc + 1 else acc) 0 l

f::Eq a => [a] -> a -> Integer
f l = (\x -> occf l x)

occM::Eq a => [a] -> [a] -> [Integer]
occM l v = map (f l) v 

main::IO()
main = do
    print $ occM [1,1,2,7,7,8,9,0,7,1] [1,7]
    