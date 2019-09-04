qSort :: (Ord a) => [a] -> [a]
qSort [] = []
qSort (s:xs) = qSort [x | x <- xs, x < s] ++ [s] ++ qSort [ x | x <- xs, x >= s]

qSort' :: (Ord a) => [a] -> [a]
qSort' [] = []
qSort' (s:xs) = qSort' [x | x <- xs, x < s] ++ [s] ++ qSort' [ x | x <- xs, x > s]

qSortInvert a = reverse (qSort a)
qSortInvert' a = reverse (qSort' a)