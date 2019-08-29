qSort :: Ord a => [a] -> [a]
qSort [] = []
qSort (x:xs) = qSort maior ++ [x] ++ qSort menor
    where
        menor  = filter (< x) xs
        maior = filter (> x) xs