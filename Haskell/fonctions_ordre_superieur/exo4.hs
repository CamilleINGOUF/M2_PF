fac'::(Integer -> Integer) -> Integer -> Integer
fac' = \rec x -> if x == 0 then 1 else x * rec (x-1)

f::Integer -> Integer
f = fac' f

main::IO()
main = do
    print $ f 5