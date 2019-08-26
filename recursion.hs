{-FUNÇÕES PRELUDE HASKELL COM RECURSÃO-}
concat1 :: [[a]] -> [a]
concat1 [] = []
concat1 (x:xs) = x ++ concat1 xs

length1 :: [a] -> Int
length1 [] = 0
length1 (x:xs) = 1 + length1 xs

head1 :: [a] -> a
head1 [] = error "LISTA VAZIA"
head1 (x:xs) = x 

last1 ::[a] -> a
last1 [] = error "LISTA VAZIA"
last1 [x] = x
last1 (x:xs) = last1 xs

tail1 :: [a] -> [a]
tail1 [] = error "LISTA VAZIA"
tail1 (x:xs) = xs

replicate1 :: Int -> a -> [a]
replicate1 0 x = []
replicate1 vezes dado = dado : replicate1 (vezes-1) dado

reverse1 :: [a] -> [a]
reverse1 [] = []
reverse1 (x:xs) = reverse1 xs ++ [x]

drop1 :: Int -> [a] -> [a]
drop1 0 x = x
drop1 _ [] = []
drop1 num (x:xs) = (drop1 (num-1) xs) 

take1 :: Int -> [a] -> [a]
take1 0 _ = []
take1 _ [] = []
take1 num (x:xs) = x : (take1 (num-1) xs)

splitAt1 :: Int -> [a] -> ([a],[a])
splitAt1 num (x:xs) = (take1 num (x:xs), drop1 num (x:xs))

sum1 :: Num a => [a] -> a
sum1 [] = 0
sum1 (x:xs) = x + sum1 xs

elem1 :: Eq a => a -> [a] -> Bool
elem1 num [] = False
elem1 num (x:xs) | num == x = True
                 | otherwise = elem1 num xs 

init1 :: [a] -> [a]
init1 [] = error "LISTA VAZIA"
init1 [x] = []
init1 (x:xs) = x : init1 xs 

product1 :: Num a => [a] -> a
product1 [] = 1
product1 (x:xs) = x * product1 xs

zip1 :: [a] -> [b] -> [(a,b)]
zip1 [] _ = []
zip1 _ [] = []
zip1 (x:xs) (y:ys) = (x,y) : zip1 xs ys

and1 :: [Bool] -> Bool
and1 [] = True
and1 (x:xs) | (x == True) && (and1 xs == True) = True
            | otherwise = False

or1 :: [Bool] -> Bool
or1 [] = False
or1 (x:xs) | (x == False) && (or1 xs == False) = False
           | otherwise = True

unzip1 :: [(a,b)] -> ([a],[b])
unzip1 [] = ([],[])
unzip1 x = (lista x, lista1 x)
-- FUNÇOES AUXILIARES UNZIP
lista :: [(a, b)] -> [a]
lista [] = []
lista ((x,y):xs) = [x] ++ lista xs  

lista1 :: [(a,b)] -> [b]
lista1 [] = []
lista1 ((x,y):xs) = [y] ++ lista1 xs
--------------------------------------

unzip2::[(Int,Int)]->([Int],[Int])
unzip2 [] = ([],[])
unzip2 ((a,b):xs) = (a : (fst (unzip2 xs)), b : (snd (unzip2 xs)))

{-OUTRAS FUNÇÕES-}
intercalaLista :: [a] -> [a] -> [a]
intercalaLista xs [] = xs
intercalaLista [] ys = ys
intercalaLista (x:xs) (y:ys) = [x,y] ++ intercalaLista xs ys

isPalindromo :: String -> Bool
isPalindromo x | x == reverse1 x = True
               | otherwise = False

calculaMaior :: [Int] -> Int
calculaMaior [x] = x
calculaMaior (x:xs) | x > calculaMaior xs = x
                    | otherwise = calculaMaior xs

retornaValor :: Int -> [Int] -> Int
retornaValor 1 (x:xs) = x
retornaValor num (x:xs) = retornaValor (num-1) xs

primeiraPalavra :: String -> String
primeiraPalavra "" = ""
primeiraPalavra (x:xs) | x == ' ' = "" 
                       | otherwise = x : primeiraPalavra xs

tiraRepetidos :: [Int] -> [Int]
tiraRepetidos [x] = [x]
tiraRepetidos [] = []
tiraRepetidos (x:y:xs) | x == y = tiraRepetidos xs 
                       | otherwise = x : y : tiraRepetidos xs

semPrimeiraPalavra :: String -> String
semPrimeiraPalavra "" = ""
semPrimeiraPalavra (x:xs) | x == ' ' = xs 
                          | otherwise = semPrimeiraPalavra xs

semPrimeiraPalavra1 :: String -> String              
semPrimeiraPalavra1 x =  drop1 (length1 (primeiraPalavra x)+1) x

semPrimeiraPalavra2 :: String -> String              
semPrimeiraPalavra2 x =  tail1 (drop1 (length1 (primeiraPalavra x)) x)

calculaDistanciaPontos :: (Float, Float) -> (Float, Float) -> Float
calculaDistanciaPontos (x1, y1) (x2, y2) = sqrt ((x1-x2)**2 + (y1-y2)**2)

distanciaPontos :: [(Float,Float)] -> Float
distanciaPontos [(x,y)] = 0
distanciaPontos [(x1,x2),(y1,y2)] = calculaDistanciaPontos (x1,y1) (x2,y2)
distanciaPontos ((x,y):xs) = distanciaPontos xs + calculaDistanciaPontos (head xs) (x,y)

pares :: [Int] -> [Int]
pares [] = []
pares (x:xs) | x `mod` 2 == 0 = x : pares xs
             | otherwise = pares xs

impares :: [Int] -> [Int]
impares [] = []
impares (x:xs) | x `mod` 2 == 1 = x : impares xs
               | otherwise = impares xs

primos :: [Int] -> [Int]
primos [] = []
primos (x:xs) | contaDivisores x == 2 = x : primos xs
              | otherwise = primos xs

contaDivisores :: Int -> Int
contaDivisores x = length1 [divisor | divisor<-[1..x], x `mod` divisor == 0] 

ehMinusculo :: Char -> Bool
ehMinusculo x = (x >= 'a') && (x <= 'z')

-- SLIDE RECURSAO PRIMITIVA AULA 07
isDigit :: Char -> Bool
isDigit x = not ((x >= 'A') && (x <= 'z')) 

somaPar :: [Int] -> Int
somaPar [] = 0
somaPar (x:xs) | ehpar x = x + somaPar xs
               | otherwise = somaPar xs
                where ehpar a = (mod a 2 == 0)

produtoImpar :: [Int] -> Int
produtoImpar [] = 1
produtoImpar (x:xs) | impar x = x * produtoImpar xs
                    | otherwise = produtoImpar xs
                     where impar a = (mod a 2 == 1)

retornaDig :: String -> String
retornaDig "" = ""
retornaDig (x:xs) | (isDigit x) && (x /= ' ') = x : retornaDig xs
                  | otherwise = retornaDig xs


mdc :: Int -> Int -> Int
mdc a 0 = a
mdc 0 b = b                                                                                                  
mdc a b = if a > b then mdc (a `mod` b) b else mdc a (b `mod` a) 

     
