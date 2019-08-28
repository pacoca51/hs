f n
    | n <= 0 = 0
    | n == 1 = 1
    | otherwise = f (n-1) + f (n-2)