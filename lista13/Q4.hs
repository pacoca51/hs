raiz :: Int -> Int
raiz n = sqrt n
    where
        sqrt x
            | x * x > n = sqrt (x - 1)
            | otherwise = x