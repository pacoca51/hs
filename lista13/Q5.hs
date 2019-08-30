removerX :: Int -> [Int] -> [Int]
removerX x y
            | y == [] = []
            | (head y) == x = (tail y)
            | otherwise = (head y):(removerX x (tail y))