f n
    | n == 0 = 1
    | otherwise = n * f (n-1)