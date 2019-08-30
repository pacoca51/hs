menor :: Int -> Int -> Int
menor x y
    | x < y  = x
    | x > y  = y
    | x == y = x

menorv :: [Int] -> Int
menorv [] = 0
menorv [x] = x
menorv (x:xs) = menor x (menorv xs)