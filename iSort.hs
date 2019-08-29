inserir :: (Ord a) => [a] -> [a]
inserir [] = []
inserir (x:xs) = inserirOrd x (inserir xs)

inserirOrd ::(Ord a) => a -> [a] -> [a]
inserirOrd x [] = [x]
inserirOrd x (y:ys)
    | x <= y = (x:y:ys)
    | otherwise = y:(inserirOrd x ys)