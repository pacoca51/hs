converteCaracter :: [Char] -> [Int]
converteCaracter xs = map ord xs

quadInt :: [Int] -> [Int]
quadInt xs = map (^2) xs

segundoPar :: [(a,b)] -> [b]
segundoPar xs = map (\(x,y) -> y) xs

tamanhoLista :: [a] -> Int
tamanhoLista xs = sum(map (\x -> snd(x,1)) xs)

dobroInt :: [Int] -> [Int]
dobroInt xs = zipWith (+) xs xs

quadIntZip :: [Int] -> [Int]
quadIntZip xs = zipWith (*) xs xs

naoContemDigito :: String -> Bool
naoContemDigito xs = not(or (map (isDigit) xs))

pegaPositivo :: [Int] -> [Int]
pegaPositivo xs = filter (>0) xs

pegaParOrd :: [(Int,Int)] -> [(Int,Int)]
pegaParOrd xs = filter(\(a,b) -> b==succ a) xs

paresQuadrado :: [(Int,Int)] -> [(Int,Int)]
paresQuadrado xs = filter (\(a,b) -> b==a*a) xs

intOrdenado :: [[Int]] -> [[Int]]
intOrdenado xs = filter (\x -> x==quicksort x) xs

quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (x:xs) = quicksort (filter (<x) xs) ++ [x] ++ quicksort (filter (x<) xs)

ultOcorre :: String -> Char -> Int
ultOcorre xs ch = if ocorrencia == [] then -1 else last ocorrencia
                  where ocorrencia = (map snd(filter(\(x,y) -> x==ch) (zip xs [1..])))

noFirst :: (a -> Bool) -> [a] -> [a]
noFirst p xs = (takeWhile (p) xs) ++ (tail (dropWhile (p) xs))

filterFirst :: Eq a => (a -> Bool) -> [a] -> [a]
filterFirst p xs = map (\(x,y) -> x)(filter(/=filtro) (zip xs [1..]))
                   where filtro = head[ (a,b) | (a,b)<-zip xs [1..], not (p a)]

filterLast :: Eq a => (a -> Bool) -> [a] -> [a]
filterLast p xs = map (\(x,y) -> x)(filter(/=filtro) (zip xs [1..]))
                   where filtro = last[ (a,b) | (a,b)<-zip xs [1..], not (p a)]

selectPosPar :: [a] -> [a]
selectPosPar xs = map (\(a,b) -> a) (filter (\(x,y) -> y `mod` 2 == 0) (zip xs [1..]))

selectPosImp :: [a] -> [a]
selectPosImp xs = map (\(a,b) -> a) (filter (\(x,y) -> y `mod` 2 == 1) (zip xs [1..]))

parImparpos :: Eq a => [a] -> ([a],[a])
parImparpos xs = (selectPosPar xs, selectPosImp xs)

dobrarInt :: [Int] -> [Int]
dobrarInt xs = zipWith (+) xsFiltrada xsFiltrada
             where xsFiltrada = filter (odd) xs

foldBool :: [Bool] -> Bool
foldBool xs = foldr (||) False xs

foldReais :: [Double] -> Double
foldReais xs = foldr (*) 1 xs

foldFat :: Int -> Int
foldFat x = foldr (*) 1 [1..x]

foldMenor :: [Int] -> Int
foldMenor xs = foldr1 min xs

foldMenorPar :: [(Int,Int)] -> (Int,Int)
foldMenorPar xs = foldr1 (\(a,b) (c,d) -> if a<c || (a==c && b<=d) then (a,b) else (c,d)) xs

somaImp :: [Int] -> Int
somaImp xs = sum(filter (odd) xs)

prodQuad :: [Int] -> Int
prodQuad xs = foldr (*) 1 (map (^2) xs)

maiorTres :: [Int] -> Int
maiorTres xs = prodQuad (filter (>3) xs)

menorElem :: [Int] -> Int
menorElem xs = head(quicksort xs)

--menorPar :: [(Int,Int)] -> (Int,Int)
--menorPar xs = (\(a,b) (c,d) -> if a<c || (a==c && b<=d) then (a,b) else (c,d)) xs

switchMap :: (a->b) -> (a->b) -> [a] -> [b]
switchMap p q [x] = []
switchMap p q (x:y:xs) = p x : q y : switchMap p q xs

primPalavra :: String -> String
primPalavra str = takeWhile (\x -> not(x `elem` [' ','\t','\n'])) str

descPrimPal :: String -> String
descPrimPal str = dropWhile (\x -> not(x `elem` [' ','\t','\n'])) str

pulaDemarcadores :: String -> String
pulaDemarcadores (x:xs) | x `elem` [' ','\t','\n'] = xs 
                        | otherwise = x:xs

listaPalavras :: String -> [Char]
listaPalavras "" = []
listaPalavras (x:xs) | not(x `elem` [' ','\t','\n']) = x : listaPalavras xs 
                     | otherwise = ' ' : listaPalavras xs

--lista :: String -> 
--lista xs = 
























