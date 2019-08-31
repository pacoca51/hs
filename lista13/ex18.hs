qSort :: (Ord a) => [a] -> [a]
qSort [] = []
qSort (s:xs) = qSort [x | x <- xs, x < s] ++ [s] ++ qSort [ x | x <- xs, x > s]

anagrams :: Int -> [String] -> String
anagrams 0 _ = "";
anagrams _ [] = "";
anagrams n ws = qSort st
                where
                    st = concat wl
                    wl = [ w | w <- ws, n == length w]
{-
anagrams 0 _ = "";
anagrams _ [] = "";
anagrams n ws = [ w | w <- ws, n == length w]
qSort anagram where anagram =
-}