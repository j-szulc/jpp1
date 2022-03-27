module HashTree where

import Hashable32
import Utils

data Tree a = Leaf Hash a | NodeTwo Hash (Tree a) (Tree a) | NodeOne Hash (Tree a)

leaf :: Hashable a => a -> Tree a
leaf a = Leaf (hash a) a

twig :: Hashable a => Tree a -> Tree a
twig t =
    let h = treeHash t in
    NodeOne (hash (h, h)) t

node :: Hashable a =>  Tree a -> Tree a -> Tree a
node t1 t2 = NodeTwo (hash (treeHash t1, treeHash t2)) t1 t2

__twigOrNode :: Hashable a =>  Tree a -> Maybe (Tree a) -> Tree a
__twigOrNode t Nothing = twig t
__twigOrNode t1 (Just t2) = node t1 t2

buildTree :: Hashable a => [a] -> Tree a
buildTree [] = error "Empty list!"
buildTree [x] = leaf x
buildTree l = do
    let buildTreeFromTrees :: Hashable a => [Tree a] -> Tree a
        buildTreeFromTrees [t] = t
        buildTreeFromTrees lt = do
            let treePairs = groupInPairs lt
            let mergedTreesList = map (uncurry __twigOrNode) treePairs
            buildTreeFromTrees mergedTreesList
    let leaves = map leaf l
    buildTreeFromTrees leaves

treeHash :: Tree a -> Hash
treeHash (Leaf h _) = h
treeHash (NodeTwo h _ _) = h
treeHash (NodeOne h _) = h

__indent :: String -> String
__indent = unlines . map (" "++) . lines

drawTree :: Show a => Tree a -> String
drawTree (Leaf h a) = showHash h ++ " " ++ show a
drawTree (NodeTwo h t1 t2) =
    let indent = unlines . map (" "++) . lines :: String -> String in
    showHash h ++ " -\n" ++
    __indent (drawTree t1) ++
    __indent (drawTree t2)
drawTree (NodeOne h t) =
    let  in
    showHash h ++ " +\n" ++
    __indent (drawTree t)

type MerklePath = [Either Hash Hash]
data MerkleProof a = MerkleProof a MerklePath

__prependProof :: MerkleProof a -> Either Hash Hash -> MerkleProof a
__prependProof (MerkleProof x path) newHead = MerkleProof x (newHead:path)

buildProof :: Hashable a => a -> Tree a -> Maybe (MerkleProof a)
buildProof x (Leaf _ y) = if hash x == hash y then Just (MerkleProof x []) else Nothing
buildProof x (NodeOne h t) =
    case buildProof x t of
        Just leftProof -> Just (__prependProof leftProof (Left h))
        Nothing -> Nothing
buildProof x (NodeTwo h t1 t2) =
    let leftPath = buildProof x t1 in
    let rightPath = buildProof x t2 in
    case (leftPath, rightPath) of
        (Just _, Just _) -> error "Element exists in both branches of the MerkleTree"
        (Just leftProof, Nothing) -> Just (__prependProof leftProof (Left h))
        (Nothing, Just rightProof) -> Just (__prependProof rightProof (Right h))
        (Nothing, Nothing) -> Nothing



-- merklePaths :: Hashable a => a -> Tree a -> [MerklePath]

-- main = putStr $ drawTree $ buildTree "fubar"
-- main = putStr $ drawTree $ buildTree "fubar"

