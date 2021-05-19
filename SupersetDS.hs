module SupersetDS where

import MiniSat
import Debug.Trace

data BinaryTree
    = Node Bool BinaryTree BinaryTree 
    | Leaf
    deriving (Show)
    
trieSetInsert :: BinaryTree -> [Lit] -> BinaryTree
-- empty case
trieSetInsert (Node _ l r) [] = Node True l r
trieSetInsert Leaf [] = Node True Leaf Leaf
trieSetInsert (Node a l r) (x:xs)
    | minisat_sign x = Node a (trieSetInsert l xs) r
    | otherwise      = Node a l (trieSetInsert r xs)
trieSetInsert Leaf (x:xs)
    | minisat_sign x = Node False (trieSetInsert (Node False Leaf Leaf) xs) Leaf
    | otherwise      = Node False Leaf (trieSetInsert (Node False Leaf Leaf) xs)


trieSetFindSuperset :: BinaryTree -> [Lit] -> [Lit] -> Bool
trieSetFindSuperset Leaf [] _ = False
trieSetFindSuperset (Node _ _ _) [] _ = True
trieSetFindSuperset Leaf _ _ = False
trieSetFindSuperset (Node a l r) (x:xs) (u:us)
    | x == u = trieSetFindSuperset r xs us
    | minisat_negate x == u = trieSetFindSuperset l xs us
    | otherwise = trieSetFindSuperset l (x:xs) us || trieSetFindSuperset r (x:xs) us