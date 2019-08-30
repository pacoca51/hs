f :: Int -> Int
f m
    | m == 0 = 8
    | m == 1 = 44
    | m == 2 = 11
    | m == 3 = 58
    | m == 4 = 0
    | otherwise = 1

algumF0 :: Int -> Bool
algumF0 c
    | c == 0 && f c /= 0 = False
    | f c == 0 = True
    | otherwise = algumF0(c-1)