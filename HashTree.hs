module HashTree where

import Data.Maybe
import Hashable32
import Utils

data Tree a = Leaf Hash a | NodeTwo Hash (Tree a) (Tree a) | NodeOne Hash (Tree a)

leaf :: Hashable a => a -> Tree a
leaf a = Leaf (hash a) a

twig :: Hashable a => Tree a -> Tree a
twig t =
  let h = treeHash t
   in NodeOne (hash (h, h)) t

node :: Hashable a => Tree a -> Tree a -> Tree a
node t1 t2 = NodeTwo (hash (treeHash t1, treeHash t2)) t1 t2

__twigOrNode :: Hashable a => Tree a -> Maybe (Tree a) -> Tree a
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

__prependEachLine :: String -> String -> String
__prependEachLine with = unlines . map (with ++) . lines

__indent = __prependEachLine "  "

drawTree :: Show a => Tree a -> String
drawTree (Leaf h a) = showHash h ++ " " ++ show a
drawTree (NodeTwo h t1 t2) =
  let indent = unlines . map (" " ++) . lines :: String -> String
   in showHash h ++ " -\n"
        ++ __indent (drawTree t1)
        ++ __indent (drawTree t2)
drawTree (NodeOne h t) =
  let
   in showHash h ++ " +\n"
        ++ __indent (drawTree t)

-- E.g. Left 0x123 means we have to go *left* node, whose *sibling* has hash 0x123
type MerklePath = [Either Hash Hash]

data MerkleProof a = MerkleProof a MerklePath

merklePaths :: Hashable a => a -> Tree a -> [MerklePath]
merklePaths x (Leaf _ y) = [[] | hash x == hash y]
merklePaths x (NodeOne h t) = [Left h : path | path <- merklePaths x t]
merklePaths x (NodeTwo h t1 t2) =
  [Left (treeHash t2) : path | path <- merklePaths x t1]
    ++ [Right (treeHash t1) : path | path <- merklePaths x t2]

buildProof :: Hashable a => a -> Tree a -> Maybe (MerkleProof a)
buildProof x t = do
  path <- maybeHead (merklePaths x t)
  Just (MerkleProof x path)

showMerklePath :: MerklePath -> String
showMerklePath [] = ""
showMerklePath ((Left x) : xs) = "<" ++ showHash x ++ showMerklePath xs
showMerklePath ((Right x) : xs) = ">" ++ showHash x ++ showMerklePath xs

-- main = putStr $ drawTree $ buildTree "fubar"

__hashPreservingOrder :: (Hashable b1, Hashable b2, Hashable a) => Either b2 a -> b1 -> Hash
__hashPreservingOrder (Left x) y = hash (y, x)
__hashPreservingOrder (Right x) y = hash (x, y)

-- Calculates root hash by travelling the MerklePath
__traverseMerklePath :: Hash -> MerklePath -> Hash
__traverseMerklePath = foldr __hashPreservingOrder

verifyProof :: Hashable a => Hash -> MerkleProof a -> Bool
verifyProof h (MerkleProof x path) = __traverseMerklePath (hash x) path == h

instance Show a => Show (Tree a) where
  show = drawTree

instance (Show a) => Show (MerkleProof a) where
  showsPrec p (MerkleProof x path) =
    showParen (p>10) $
    showString "MerkleProof " .
    showsPrec 11 x .
    showChar ' ' .
    showString (showMerklePath path)


-- |
-- >>> putStr $ drawTree $ buildTree "fubar"
-- 0x2e1cc0e4 -
--   0xfbfe18ac -
--     0x6600a107 -
--       0x00000066 'f'
--       0x00000075 'u'
--     0x62009aa7 -
--       0x00000062 'b'
--       0x00000061 'a'
--   0xd11bea20 +
--     0x7200b3e8 +
--       0x00000072 'r'

-- |
-- >>> map showMerklePath  $ merklePaths 'i' $ buildTree "bitcoin"
-- ["<0x5214666a<0x7400b6ff>0x00000062",">0x69f4387c<0x6e00ad98>0x0000006f"]

-- |
-- >>> buildProof 'i' $ buildTree "bitcoin"
-- Just (MerkleProof 'i' <0x5214666a<0x7400b6ff>0x00000062)

-- |
-- >>> buildProof 'e' $ buildTree "bitcoin"
-- Nothing

-- |
-- >>> let t = buildTree "bitcoin"
-- >>> let proof = buildProof 'i' t
-- >>> verifyProof (treeHash t) <$> proof
-- Just True
-- >>> verifyProof 0xbada55bb <$> proof
-- Just False