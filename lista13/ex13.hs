divMod' :: Int -> Int -> (Int, Int)
divMod' x y
    | x <= 0 = (0,0)
    | y <= 0 = (0,0)
    | otherwise = (divxy,modxy)
    where
        divxy = div x y
        modxy = mod x y