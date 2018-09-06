import Control.Monad
import System.Exit

testValue :: Int -> Int -> String
testValue x v
    | x > v = "Too high."
    | x < v = "Too low."
    | otherwise = "Just right."

game = do
    print "Guess my number : "
    s <- getLine
    let ss = read s::Int
    print $ testValue ss 7
    when (ss == 7) $ exitSuccess

main = forever game