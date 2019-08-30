removerTX :: Int -> [Int] -> [Int]
removerTX x y
            | y == [] = []
            | (head y) == x = (removerTX x (tail y))
            | otherwise = (head y):(removerTX x (tail y))