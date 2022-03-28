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
maybeHead (x:_) = Just x
maybeHead _ = Nothing

justOrError :: Maybe a -> String ->  a
justOrError (Just x) _ = x
justOrError Nothing errorStr = error errorStr

groupInPairs :: [a] -> [(a, Maybe a)]
groupInPairs [] = []
groupInPairs [x] = [(x, Nothing)]
groupInPairs (x:y:tail) = (x, Just y) : groupInPairs tail