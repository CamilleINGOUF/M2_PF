sr:: (Integral a, Fractional b) => a -> b
sr 0 = 0.8
sr n = 0.6 * m * (m - 1)
    where m = (sr (n - 1)) 

main = do
    print $ sr 1000