module Maybe2 where

-- Option.map
mapJust :: (a->b) -> Maybe a -> Maybe b
mapJust f Nothing = Nothing
mapJust f (Just x) = Just (f x)

-- Option.apply == mapMaybe
-- Option.get == fromJust
-- Option.set == fromMaybe
-- Option.setmap == maybe

-- Option.zap
zap :: (a -> Bool) -> Maybe a -> Maybe a
zap f Nothing = Nothing
zap f (Just x) = if f x then Nothing else Just x

-- Option.compare
compare :: Ord a => Maybe a -> Maybe a -> Ordering
compare (Just a) (Just b) = Prelude.compare a b
compare Nothing Nothing = EQ
compare (Just a) Nothing = LT
compare Nothing (Just a) = GT

-- Option.pred == isJust
-- Option.filter == catMaybes

-- Option.to_string
to_string :: (a -> String) -> Maybe a -> String
to_string f Nothing = "Nothing"
to_string f (Just a) = "Just " ++ f a
