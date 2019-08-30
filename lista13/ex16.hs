sublista :: [Char] -> [Char] -> Bool
sublista a b
    | a == [] && b == [] = True
    | a /= [] && b == [] = False
    | a == [] && b /= [] = True
    | (head a) == (head b) = (sublista (tail a) (tail b))
    | otherwise = (sublista a (tail b))