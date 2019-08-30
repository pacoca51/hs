fn :: [Int] -> [Int]
fn xs = [x | (x,y) <- zip xs [0..], notElem x (take y xs)]

ins :: Int -> [Int] -> [Int]
ins x [] = [x]
ins x (y:ys)
    | x <= y = x:y:ys
    | otherwise = [y] ++ (ins x ys)
        
insSort :: [Int] -> [Int]
insSort (x:xs)
    | xs == [] = [x]
    | otherwise =  fn (ins x (insSort xs))
