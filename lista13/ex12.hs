mdc :: Int -> Int -> Int
mdc a b
    | a == 0 = b
    | b == 0 = a
    | modAB == 0 = b
    | otherwise = mdc b modAB
    where
        modAB = mod a b