fibo:: (Integral a) => a -> a
fibo 0 = 0
fibo 1 = 1
fibo n = fibo (n - 2) + fibo (n - 1)

fibo_t:: (Integral b) => b -> b -> b -> b
fibo_t 0 _ n = n
fibo_t 1 _ n = n
fibo_t v m n = fibo_t (v-1) n (m + n)

main = do
    print $ fibo 10
    print $ fibo_t 100000 0 1