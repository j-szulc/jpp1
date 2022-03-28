module HashTree where

import Hashable32
import Utils
import Data.Maybe


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

-- E.g. Left 0x123 means we have to go *left* node, whose *sibling* has hash 0x123
type MerklePath = [Either Hash Hash]
data MerkleProof a = MerkleProof a MerklePath

merklePaths :: Hashable a => a -> Tree a -> [MerklePath]
merklePaths x (Leaf _ y) = [[] | hash x == hash y]
merklePaths x (NodeOne h t) = [Left h:path | path <- merklePaths x t]
merklePaths x (NodeTwo h t1 t2) =
     [Left  (treeHash t2):path | path <- merklePaths x t1] ++
     [Right (treeHash t1):path | path <- merklePaths x t2]

buildProof :: Hashable a => a -> Tree a -> Maybe (MerkleProof a)
buildProof x t = do
    path <- maybeHead (merklePaths x t)
    Just (MerkleProof x path)

showMerklePath :: MerklePath -> String
showMerklePath [] = ""
showMerklePath ((Left x):xs) = "<" ++ showHash x ++ showMerklePath xs
showMerklePath ((Right x):xs) = ">" ++ showHash x ++ showMerklePath xs

-- main = putStr $ drawTree $ buildTree "fubar"

__hashPreservingOrder :: (Hashable b1, Hashable b2, Hashable a) => Either b2 a -> b1 -> Hash
__hashPreservingOrder (Left x) y = hash (y,x)
__hashPreservingOrder (Right x) y = hash (x,y)

-- Calculates root hash by travelling the MerklePath
__traverseMerklePath :: Hash -> MerklePath -> Hash
__traverseMerklePath = foldr __hashPreservingOrder

verifyProof :: Hashable a => Hash -> MerkleProof a -> Bool
verifyProof h (MerkleProof x path) = __traverseMerklePath (hash x) path == h

instance Show a => Show (Tree a) where
    show = drawTree

instance Show (MerkleProof a) where
    show (MerkleProof x path) = showMerklePath path

{-
t = buildTree "bitcoin"
proof = buildProof 'i' t
test1 = verifyProof (treeHash t) <$> proof
test2 = verifyProof 0xbada55bb <$> proof
-}
