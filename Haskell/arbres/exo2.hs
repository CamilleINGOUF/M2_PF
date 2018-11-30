import Tree

data Operator = Plus | Minus | Times | Divide
data Lexeme = Number Double | Op Operator | Var String 

instance Show Operator where
  show Plus = "+"
  show Minus = "-"
  show Times = "*"
  show Divide = "/"

instance Show Lexeme where
  show (Number a) = show a
  show (Op a) = show a
  show (Var a) = a

assoc::[(String, Double)] -> String -> Double
assoc [] _ = error "Not Found"
assoc ((k,v):xs) x
  | k == x = v
  | otherwise = assoc xs x

evalVar::Tree Lexeme -> [(String, Double)] -> Double
evalVar (Node (Number n) _ _) _ = n
evalVar (Node (Var v) _ _) vars = assoc vars v
evalVar (Node (Op Times) lr rt) vars = (evalVar lr vars) * (evalVar rt vars)
evalVar (Node (Op Plus) lr rt) vars = (evalVar lr vars) + (evalVar rt vars)
evalVar (Node (Op Minus) lr rt) vars = (evalVar lr vars) - (evalVar rt vars)
evalVar (Node (Op Divide) lr rt) vars = (evalVar lr vars) / (evalVar rt vars)


main::IO()
main = do
    print a
    print $ evalVar a [("x",74.0), ("y",3.6)]
  where 
    a = consTree (Op Times) l r
    r = (consTree (Op Minus) (consLeaf (Number 1.2)) (consLeaf (Var "y")))
    l = (consTree (Op Plus) (consLeaf (Var "x") ) (consTree (Op Divide) (consLeaf (Number 45)) (consLeaf (Number 7))))