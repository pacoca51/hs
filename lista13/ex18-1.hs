anagrams :: Int -> [String] -> String
anagrams n ws = dictAn (qSort2 ws) (map qSort2 (qSort2 ws)) (qSort (foldr1 (++) (map createAn (removeDuplicate (map qSort2 (map downL (filter len ws)))))))
    where
        len str = length str == n
        qSort xs
            | null xs = []
            | otherwise = qSort menor ++ [pivo] ++ qSort maior
                where
                    pivo = head xs
                    maior = filter (> pivo) (tail xs)
                    menor = filter (< pivo) (tail xs)
        qSort2 xs
            | null xs = []
            | otherwise = qSort2 menor ++ [pivo] ++ qSort2 maior
                where
                    pivo = head xs
                    maior = filter (>= pivo) (tail xs)
                    menor = filter (< pivo) (tail xs)
        removeDuplicate list
            | null list = []
            | elem (last list) (init list) = removeDuplicate (init list)
            | otherwise = removeDuplicate (init list) ++ [last list]
        downL word
            | word == [] = []
            | elem (head word) ['A'..'Z'] = ['a'..'z']!!(head([i | (i, l) <- zip [0..] ['A'..'Z'], l == head word])) : downL (tail word)
            | otherwise = head word : downL (tail word)
        dictAn ori oriSort allAnagrams
            | null allAnagrams = ""
            | otherwise = (head allAnagrams) ++ ": " ++ whoIsAnagram ori oriSort (qSort2 (head allAnagrams)) ++ "\n" ++ dictAn ori oriSort (tail allAnagrams)
            where
                whoIsAnagram ori oriSort word
                    | null oriSort = ""
                    | head oriSort == word && notElem word (tail oriSort) = head ori
                    | head oriSort == word = head ori ++ ", " ++ whoIsAnagram (tail ori) (tail oriSort) word
                    | otherwise = whoIsAnagram (tail ori) (tail oriSort) word
        createAn word = nextAll [word] 0
            where
                nextAll list n
                    | null list = []
                    | length (head list) == n = list ++ nextAll (tail list) n
                    | otherwise = nextAll (nextFix (head list) n) (n + 1) ++ nextAll (tail list) n
                    where
                        nextFix str n = map putFix (nextEach variablePart variablePart)
                            where
                                nextEach str word
                                    | next str /= word = str : nextEach (next str) word
                                    | otherwise = [str]
                                    where
                                        next str = last str : init str
                                variablePart = drop n str
                                putFix x = fixedLetters ++ x
                                    where
                                        fixedLetters = take n str