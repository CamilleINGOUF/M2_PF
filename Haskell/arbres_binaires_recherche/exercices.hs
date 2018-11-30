data Tree a = Empty
          | Node a (Tree a) (Tree a) deriving (Show)

-- instance (Show a) => Show (Tree a) where
--   show Empty = "[]"
--   show (Node a Empty Empty) = show a
--   show (Node a l r) = "(" ++ show l ++ " " ++ show a ++ " " ++ show r ++ ")"

root::Tree a -> a
root Empty = error "No root"
root (Node eleme _ _) = eleme

left::Tree a -> Tree a
left (Node _ l _) = l

right::Tree a -> Tree a
right (Node _ _ r) = r

isLeaf::Tree a -> Bool
isLeaf (Node _ Empty Empty) = True
isLeaf _ = False

insert::(Ord a) => Tree a -> a -> Tree a
insert Empty a = (Node a Empty Empty)
insert (Node eleme lt rt) a
  | a > eleme = (Node eleme lt (insert rt a))
  | otherwise = (Node eleme (insert lt a) rt)

search::(Ord b) => Tree b -> b -> Bool
search Empty _ = False
search (Node eleme lt rt) a
  | a > eleme = search rt a
  | a < eleme = search lt a
  | otherwise = True

cutMax::(Ord a) => Tree a -> (Tree a, a)
cutMax Empty = error "Empty"
cutMax (Node eleme Empty Empty) = (Empty, eleme)
cutMax (Node eleme lt rt) = (Node eleme lt tree,maxi)
    where (tree,maxi) = cutMax rt

deleteElem::(Ord a, Eq a) => Tree a -> a -> Tree a
deleteElem Empty _ = Empty
deleteElem (Node eleme Empty rt) a
  | eleme == a = rt
  | otherwise = (Node eleme Empty (deleteElem rt a))
deleteElem (Node eleme lt rt) a
  | eleme < a = (Node eleme lt (deleteElem rt a))
  | eleme > a = (Node eleme (deleteElem lt a) rt)
  | otherwise = (Node maxi tree rt)
  where (tree, maxi) = cutMax lt

rl::(Ord a) => Tree a -> Tree a
rl Empty = Empty
rl (Node eleme lt rt) = Node (root rt) (Node eleme lt (left rt)) (right rt)

rr::(Ord a) => Tree a -> Tree a
rr Empty = Empty
rr (Node eleme lt rt) = Node (root lt) lt (Node eleme (right lt) rt)

-- Fonction qui compte le nombre d’éléments dans un intervalle donné [a, b].
countRange::(Ord a) => Tree a -> (a,a) -> Int
countRange Empty _ = 0
countRange (Node eleme lt rt) (mini, maxi)
  | mini > maxi = error "Mini <= Maxi"
  | eleme < mini = countRt
  | eleme > maxi = countLt
  | otherwise = 1 + countLt + countRt
  where countLt = countRange lt (mini, maxi)
        countRt = countRange rt (mini, maxi)

insertPoint::Tree (Int, Int) -> (Int, Int) -> Bool -> Tree (Int, Int)
insertPoint Empty xy _ = (Node xy Empty Empty)
insertPoint (Node (x,y) lt rt) (xx,yy) True
  | xx > x = (Node (x,y) lt (insertPoint rt (xx,yy) False))
  | otherwise = (Node (x,y) (insertPoint lt (xx,yy) False) rt)
insertPoint (Node (x,y) lt rt) (xx,yy) False
  | yy > y = (Node (x,y) lt (insertPoint rt (xx,yy) True))
  | otherwise = (Node (x,y) (insertPoint lt (xx,yy) True) rt)


main::IO()
main = do
  print $ p4
  -- print $ t6
  -- print $ countRange t6 (5,45)
  where p = (Node (5,5) Empty Empty)
        p2 = insertPoint p (6,5) True
        p3 = insertPoint p2 (5,6) True
        p4 = insertPoint p3 (7,4) True
        -- t = (Node 12 Empty Empty)
        -- t2 = insert t 20
        -- t3 = insert t2 4
        -- t4 = insert t3 6
        -- t5 = insert t4 45
        -- t6 = insert t5 35