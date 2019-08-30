prefix:: String -> String -> Bool
prefix s1 s2
    | null s1 = True
    | null s2 = False
    | head s1 == head s2 = prefix (tail s1) (tail s2)
    | otherwise = False

subSeq :: String -> String -> Bool
subSeq s1 s2
    | null s1 = True
    | null s2 = False
    | prefix s1 s2 = True
    | otherwise = (subSeq s1) (tail s2)