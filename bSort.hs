bolha [] = []
bolha x = bolhaOrd x (length x)

bolhaOrd x 0 = x
bolhaOrd x n = bolhaOrd (trocar x) (n-1)

trocar [x] = [x]
trocar (x:y:zs)
    | x > y = y:trocar (x:zs)
    | otherwise = x:trocar (y:zs)