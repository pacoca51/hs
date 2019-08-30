f :: Int -> Int
f m
    | m == 0 = 8
    | m == 1 = 44
    | m == 2 = 11
    | otherwise = 0

maiorF :: Int -> Int
maiorF n
    | n == 0 = f 0
    | otherwise = max (maiorF(n-1)) (f n)