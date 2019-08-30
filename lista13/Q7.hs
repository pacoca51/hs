inverte :: [Char] -> [Char]
inverte s
    | null s = s
    | otherwise = (last s) : inverte (init s)