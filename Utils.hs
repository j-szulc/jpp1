module Utils where

fromEither :: Either a a -> a
fromEither = either id id

-- fromEither (Left a) = a
-- fromEither (Right a) = a

isJust :: Maybe a -> Bool
isJust = maybe False (const True)

-- isJust Nothing = False
-- isJust _ = True

fromMaybe :: a -> Maybe a -> a
fromMaybe def = maybe def id

-- fromMaybe def Nothing = def
-- fromMaybe def (Just a) = a

maybeHead :: [a] -> Maybe a
maybeHead (x : _) = Just x
maybeHead _ = Nothing

justOrError :: Maybe a -> String -> a
justOrError (Just x) _ = x
justOrError Nothing errorStr = error errorStr

groupInPairs :: [a] -> [(a, Maybe a)]
groupInPairs [] = []
groupInPairs [x] = [(x, Nothing)]
groupInPairs (x : y : tail) = (x, Just y) : groupInPairs tail

curry3 :: (t1 -> t2 -> t3 -> t4) -> (t1, t2, t3) -> t4
curry3 f (a, b, c) = f a b c

uncurry3 :: ((a, b, c) -> t) -> a -> b -> c -> t
uncurry3 f a b c = f (a, b, c)

prependEachLine :: String -> String -> String
prependEachLine with = unlines . map (with ++) . lines

showsPrepended :: String -> String -> ShowS
showsPrepended prependWith a b = prependEachLine prependWith a ++ b

showsIndented :: String -> ShowS
showsIndented = showsPrepended "  "