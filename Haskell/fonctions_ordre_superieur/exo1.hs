map'::(a -> b) -> [a] -> [b]
map' f l = [f x | x <- l]

filter'::(a -> Bool) -> [a] -> [a]
filter' f l = [x | x <- l , f x]

main::IO()
main = do
    print $ map' (+1) [1..10]
    print $ map' (reverse) [[1..5],[6..9],[5..7]]
    print $ map' (\x -> x - 1) [1..10]
    print $ filter' (even) [1..10]