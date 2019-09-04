f n
    | n < 0 = 0
    | n == 0 = 1
    | otherwise = n * f (n-1)

mult x = [f x | x <- x]