ins :: Int -> [Int] -> [Int]
ins x [] = [x]
ins x (y:ys)
    | x >= y = x:y:ys
    | otherwise = [y] ++ (ins x ys)
    
insSort :: [Int] -> [Int]
insSort (x:xs)
    | xs == [] = [x]
    | otherwise = ins x (insSort xs)
