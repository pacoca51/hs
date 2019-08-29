menor :: (Ord a) => [a] -> a
menor [] = undefined
menor [x] = x
menor (x:xs)
    | x <= (menor xs) = x
    | otherwise = menor xs