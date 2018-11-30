import Tree

maxT::(Ord a) => Tree a -> a
maxT (Node elem Empty Empty) = elem
maxT (Node elem lt Empty) = max elem (maxT lt)
maxT (Node elem Empty rt) = max elem (maxT rt)
maxT (Node elem lt rt) = max elem (max (maxT lt) (maxT rt))

maxTOrd::(Ord a) => Tree a -> a
maxTOrd (Node elem Empty Empty) = elem
maxTOrd (Node elem _ rt) = maxTOrd rt 

main::IO()
main = do
  print d
  print $ maxT d
  print $ maxTOrd d
  where a = consLeaf 6
        b = consLeaf 9
        c = consTree 5 (consLeaf 4) a
        d = consTree 7 c b