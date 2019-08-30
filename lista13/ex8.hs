fn :: Int -> [Int] -> [Int]
fn x y
            | y == [] = []
            | (head y) == x = (fn x (tail y))
            | otherwise = (head y):(fn x (tail y))

unique :: [Int] -> [Int]
unique xs = [x | (x,y) <- zip xs [0..], notElem x ((take y xs) ++ (drop (y + 1) xs))]

uniquer :: [Int] -> [Int]
uniquer xs
    | xs == [] =[]
    | elem (head xs) (tail xs) = uniquer (fn (head xs) xs)
    | otherwise = (head xs):(uniquer (tail xs))
