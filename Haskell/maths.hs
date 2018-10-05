power:: (Fractional a, Integral b) => a -> b -> a
power _ 0 = 1
power x y = (power x (y-1)) * x

facto:: (Integral a) => a -> a
facto 1 = 1
facto x = (facto (x-1)) * x

pgcd::(Integral a) => a -> a -> a
pgcd a 0 = a
pgcd a b = pgcd b (mod a b) 

power_term::(Fractional a, Integral b) => a -> b -> a -> a
power_term _ 0 acc = acc
power_term x n acc = power_term x (n-1) (acc * x)

facto_term::(Integral a) => a -> a -> a
facto_term 1 acc = acc
facto_term x acc = facto_term (x-1) (acc*x) 

main = do
    print $ power_term 5 2 1
    print $ power 5 2
    print $ facto 4
    print $ facto_term 5 1
    print $ pgcd 70 56