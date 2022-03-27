module HashTree where

import Hashable32

data Tree a = Leaf Hash a | Node Hash (Tree a) (Tree a)

leaf :: Hashable a => a -> Tree a
leaf a = Leaf (hash a) a

-- twig :: Hashable a => Tree a -> Tree a

node :: Hashable a =>  Tree a -> Tree a -> Tree a
node t1 t2 = Node (hash (treeHash t1, treeHash t2)) t1 t2

buildTree :: Hashable a => [a] -> Tree a
buildTree [] = error "Empty list!"
buildTree [x] = leaf x
buildTree l = do
    let buildTreeFromTrees :: Hashable a => [Tree a] -> Tree a
        buildTreeFromTrees [t] = t
        buildTreeFromTrees lt = do
        let groupInPairs [] = []
            groupInPairs [x] = [(x,x)]
            groupInPairs (x:y:tail) = (x,y) : groupInPairs tail
        let treePairs = groupInPairs lt
        let mergedTreesList = map (uncurry node) treePairs
        buildTreeFromTrees mergedTreesList
    let leaves = map leaf l
    buildTreeFromTrees leaves

treeHash :: Tree a -> Hash
treeHash (Leaf h _) = h
treeHash (Node h _ _) = h

drawTree :: Show a => Tree a -> String
drawTree (Leaf h a) = showHash h ++ " " ++ show a
drawTree (Node h t1 t2) =
    let indent = unlines . map (" "++) . lines :: String -> String in
    showHash h ++ " -\n" ++
    indent (drawTree t1) ++
    indent (drawTree t2)

main = putStr $ drawTree $ buildTree "fubar"