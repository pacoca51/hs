mdc :: Int -> Int -> Int
mdc a b 
    | b == 0 = a
    | a > b = (mdc (a-b) b)
    | otherwise = mdc a (mod a b)