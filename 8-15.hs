safeDiv :: Integral t =>  Maybe t -> Maybe t -> Maybe t
safeDiv _ Nothing = Nothing
safeDiv Nothing _ = Nothing
safeDiv (Just x) (Just y)
    | y /= 0 = Just (x `div` y)
    | otherwise = Nothing
-----------------------------------------------------------
{-safeTail Nothing = Nothing
safeTail (Just l)
    | l /= [] = Just (tail l)
    | otherwise = Nothing-}
safeTail :: Eq t => [t] -> Maybe [t]
safeTail l
    | l /= [] = Just (tail l)
    | otherwise = Nothing
-----------------------------------------------------------
applyMaybe::Maybe a -> (a->Maybe b) -> Maybe b
applyMaybe Nothing f = Nothing
applyMaybe (Just x) f = f x
-----------------------------------------------------------