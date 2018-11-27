data Tree a = Empty
          | Node a (Tree a) (Tree a)

instance (Show a) => Show (Tree a) where
  show Empty = "[]"
  show (Node a l r) = "(" ++ show a ++ " - " ++ show l ++ " " ++ show r ++ ")"

isEmpty::Tree a -> Bool
isEmpty (Empty) = True
isEmpty _ = False

empty::Tree a
empty = Empty

consTree :: a -> Tree a -> Tree a -> Tree a
consTree v l r = (Node v l r)

root :: Tree a -> a
root (Node r _ _) = r

left::Tree a -> Tree a
left (Node _ l _) = l

right::Tree a -> Tree a
right (Node _ _ r) = r

isLeaf::Tree a -> Bool
isLeaf (Node _ Empty Empty) = True
isLeaf _ = False

consLeaf::a -> Tree a
consLeaf v = (Node v Empty Empty)

search::(Eq a) => Tree a -> a -> Bool
search Empty _ = False
search (Node v l r) s
  | v == s = True
  | otherwise = search l s || search r s

searchOrd::(Ord a) =>Tree a -> a -> Bool
searchOrd Empty _ = False
searchOrd (Node v l r) s
  | s > v = searchOrd r s
  | s < v = searchOrd l s
  | otherwise = True

countNode::Tree a -> Int
countNode Empty = 0
countNode (Node _ l r) = 1 + a + b
  where a = countNode l
        b = countNode r

sumTree::Tree Int -> Int
sumTree Empty = 0
sumTree (Node v l r) = v + (sumTree l) + (sumTree r)

main::IO()
main = do
  print d
  print $ isEmpty a
  print $ isEmpty b
  print $ isLeaf a
  print $ isLeaf c
  print $ search d 7
  print $ countNode d
  print $ sumTree d
  where a = consLeaf 5
        b = empty
        c = consTree 5 a b
        d = consTree 6 a c