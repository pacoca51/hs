f :: Int -> Bool
f m
    | m == 0 = True
    | m == 1 = False
    | m == 2 = False
    | m == 3 = False
    | m == 4 = True
    | otherwise = False

algumFentre :: Int -> Bool
algumFentre d
    | d == 0 && f d == False = False
    | f d == True = True
    | otherwise = algumFentre(d-1)
