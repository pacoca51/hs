pot :: Int -> Int -> Int
pot m n
    | n == 0 = 1
    | otherwise = m * pot m (n-1)