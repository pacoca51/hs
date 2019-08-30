f :: Int -> Int -> Int
f x y
    | x < y  = x
    | x > y  = y
    | x == y = x

menor :: [Int] -> Int
menor [] = undefined
menor [x] = x
menor (x:xs) = f x (menor xs)
