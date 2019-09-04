aux :: (Int, Int) -> (Int, Int)
aux (n, m) = (n+1,m)

divMod' :: Int -> Int -> (Int, Int)
divMod' x y 
    | x < y = (0, x)
    | otherwise = aux (divMod' (x-y) y)