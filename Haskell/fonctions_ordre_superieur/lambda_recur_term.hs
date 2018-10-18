len::[a] -> Integer
len = \l -> case l of
    (_:t) -> 1 + len t
    [] -> 0

pow::Integer -> Integer -> Integer
pow = \x -> \n -> case n of 
    0 -> 1
    otherwise -> x*pow x (n-1)

main::IO()
main = do
    print $ len [1..10]
    print $ len ['a'..'z']
    print $ pow 5 2