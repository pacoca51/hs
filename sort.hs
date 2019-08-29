selecionar :: (Ord a) => [a]->[a]
selecionar [] = []
selecionar xs = [x] ++ selecionar (remove x xs)
where x = menor xs

remover:: (Ord a) => a -> [a] -> [a]
remover a [] = []
remover a (x:xs)
    | a == x = xs
    | otherwise = x:(remove a xs)

menor :: (Ord a) => [a] -> a
menor [] = undefined
menor [x] = x
menor (x:xs)
    | x <= (menor xs) = x
    | otherwise = menor xs