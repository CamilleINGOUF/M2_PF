der'::Fractional a => (a -> a) -> a -> a -> a
der' f h = \x -> (f(x + h) - f(x))/h

df::Fractional a => a -> a
df = der' (\x -> x^2) 0.01

desc_gradient::(Ord a, Num a) => (a -> a) -> a -> a -> a -> a
desc_gradient df' x0 nu epsilon = 
    if abs (df' x0) < epsilon
    then x0
    else desc_gradient df' (x0 - nu * (df' x0)) nu epsilon

main::IO()
main = do
    print $ df 5
    print $ desc_gradient df 5 0.01 0.001