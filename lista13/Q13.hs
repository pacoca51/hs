aux :: (Int, Int) -> (Int, Int)
aux (n, m) = (n+1,m)

divModder :: Int -> Int -> (Int, Int)
divModder x y 
    | x < y = (0, x)
    | otherwise = aux (divMod (x-y) y)