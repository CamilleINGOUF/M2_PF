y::(a -> a) -> a
y f = f (y f)

premier::a -> b -> a
premier a b = a

main::IO()
main = do
    print $ take 10 (y (1:))
    print $ y (premier 42)
