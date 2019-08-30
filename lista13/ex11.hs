f :: Int -> Int -> Int
f x y
    | x < y  = x
    | x > y  = y
    | x == y = x

menor :: [Int] -> Int
menor [] = 0
menor [x] = x
menor (x:xs) = f x (menor xs)
