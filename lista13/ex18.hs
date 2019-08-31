remdup :: [String] -> [String]
remdup xs = [x | (x,y) <- zip xs [0..], notElem x (take y xs)]


listToStr :: String -> [String] -> String
listToStr s [] = []
listToStr s [x] = "(" ++ s ++ x ++ ") "
listToStr s (x:xs) = "(" ++ s ++ (x ++ ") ") ++ (listToStr s xs)

anag :: String -> [String]
anag [] = [[]]
anag xs = [ x:ys | (x,a) <- select xs, ys <- anag a]
  where select []     = []
        select (x:xs) = (x,xs) : [ (y,x:ys) | (y,ys) <- select xs ]

lToS :: [String] -> String
lToS a = (listToStr ((head a) ++ " -> ") a)

anagrams :: Int -> [String] -> String
anagrams _ [] = []
anagrams a b
    | (length (head b)) == a = lToS(remdup(anag (head b))) ++ (anagrams a (tail b))
    | otherwise = (anagrams a (tail b))
