import Control.Monad
import System.Exit
import System.Random

game :: Int -> Int -> IO()
game _ 0 = putStrLn "lost."
game v x
    | ss > v = putStrLn "Too high."
    | ss < v = putStrLn "Too low."
    | ss == v = putStr "Just right."
    | otherwise = (game v (x-1))
    where ss = askNumber

askNumber :: IO Int
askNumber = do
    putStrLn "Give number : "
    n <- getLine
    return (read n::Int)

main = do
    value <- randomRIO(1,100) :: IO Int
    game value 5