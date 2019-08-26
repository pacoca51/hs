paraMaiusculo :: Char -> Char
paraMaiusculo c | ehMinusculo c = chr ((ord c) - (ord 'a') + (ord 'A'))
                | otherwise = c
-- SLIDE DE TIPOS E DEFINICOES AULA 3
ehMinusculo :: Char -> Bool
ehMinusculo x = (x >= 'a') && (x <= 'z')

infixl 7 #
(#) :: Bool -> Bool -> Bool
x # y = (x || y) && not (x && y)

infixl 7 &&&
(&&&) :: Int -> Int -> Int
x &&& y = 2¨(x+++y)

infixl 7 +++
(+++) :: Int -> Int -> Int
x +++ y = x+y

infixl 7 ***
(***) :: Int -> Int -> Int
x *** y = (x¨y)¨(x¨y)

infixl 7 ¨
(¨) :: Int -> Int -> Int
x ¨ y = x*y

nAnd :: Bool -> Bool -> Bool
nAnd True True = False
nAnd _ _ = True

funcaoQuadrada a b c | a/= 0 && delta >= 0 = (raizMaior, raizMenor)
                     | otherwise = error "Raizes Inexistentes"
                     where delta = b^2 - 4*a*c
                           raizMaior = (negate b + sqrt(delta)) / (2*a)
                           raizMenor = (negate b - sqrt(delta)) / (2*a)


multiplo :: Int -> String
multiplo x | (x `mod` 3 == 0) && (x `mod` 5 == 0) = "multiplo de 3 e 5"
           | (x `mod` 3 == 0) = "multiplo de 3"
           | (x `mod` 5 == 0) = "multiplo de 5"
           | otherwise = "n eh multiplo de 3 nem de 5"

formaGeometrica :: Int -> Int -> String
formaGeometrica x y | (x <= 0) || (y <= 0) = error "LADO NEGATIVO OU NULO"
                    | x == y = "Quadrado"
                    | x > y || y > x = "Retangulo"

calculaArea :: Int -> Int -> Int
calculaArea x y | (x <= 0) || (y <= 0) = error "LADO NEGATIVO OU NULO"
                | otherwise = (x*y)

areaCuboide :: Int -> Int -> Int -> Int
areaCuboide x y z = 2 * (calculaArea x y + calculaArea x z + calculaArea y z)

areaCuboide1 :: Int -> Int -> Int -> Int
areaCuboide1 x y z = 2 * (calculaArea1 x y + calculaArea1 x z + calculaArea1 y z) 
                     where calculaArea1 x y | (x <= 0) || (y <= 0) = error "LADO NEGATIVO OU NULO"
                                            | otherwise = (x*y) 

areaCuboide2 :: Int -> Int -> Int -> Int
areaCuboide2  x y z = let calculaArea1 x y | (x <= 0) || (y <= 0) = error "LADO NEGATIVO OU NULO"
                                           | otherwise = (x*y) 
                      in 2 * (calculaArea1 x y + calculaArea1 x z + calculaArea1 y z) 

{- branco-paz, amarelo-alegria,
verde-esperança , azul - tranquilidade, vermelho – paixão e qualquer outra
– desculpa.-}

cor :: String -> String
cor "branco" = "paz"
cor "amarelo" = "alegria"
cor "azul" = "tranquilidade"
cor "verde" = "esperança"
cor "vermelho" = "paixao"
cor _ = "Nao significa nada hahah"

-- SLIDE DE TUPLAS E LISTA AULA 4
maxOcorre :: Int -> Int -> (Int,Int)
maxOcorre x y | x == y = (x, 2)
              | otherwise = ((max x y), 1) 


maxiOcorre2 :: Int -> Int -> Int -> (Int,Int)
maxiOcorre2 x y z = (maior, (if x == maior then 1 else 0) + (if y == maior then 1 else 0) + (if z == maior then 1 else 0))
                   where maior = max (max x y) z

ordemCrescente :: (Int,Int,Int) -> (Int,Int,Int)
ordemCrescente (a,b,c) | (a <= b) && (b <= c) = (a,b,c)
                       | (a <= c) && (c <= b) = (a,c,b)
                       | (b <= c) && (c <= a) = (b,c,a)
                       | (b <= a) && (a <= c) = (b,a,c)
                       | (c <= a) && (a <= b) = (c,a,b)
                       | (c <= b) && (b <= a) = (c,b,a)


ehPar :: Int -> Bool
ehPar x = (x `mod`2 == 0)

verificaImpares :: [Int] -> Bool
verificaImpares x = ([] == [ pares | pares<-x, ehPar pares])

imparesTriplicados :: [Int] -> [Int]
imparesTriplicados xs = [ 3*impar | impar<-xs, not (ehPar impar)]

transformaMaisculo :: String -> String
transformaMaisculo x = [ paraMaiusculo a | a<-x]

media :: [Double] -> Double
media xs = sum xs / (fromIntegral(length xs))

mediaNotas :: [Double] -> [Double]
mediaNotas xs = [nota | nota<-xs, nota > media xs]

junta :: String -> String -> String -> String
junta um dois tres = um ++ "\n" ++ dois ++ "\n" ++ tres ++ "\n"

combNomeAdj ::[String] -> [String] -> [String]
combNomeAdj nome adj = concat [[n ++ " " ++ a |  a<-adj] |n<-nome]


-- SLIDE 7 RECURSAO PRIMITIVA

type Pessoa = String
type Livro = String
type Emprestimo = [(Pessoa, Livro)]

erivelton:: Pessoa
sanny:: Pessoa
leticia:: Pessoa
pedro:: Pessoa
lucas:: Pessoa
gustavo:: Pessoa

erivelton = "erivelton"
sanny = "sanny"
leticia = "leticia"
pedro = "pedro"
lucas = "lucas"
gustavo = "gustavo"

a:: Livro
b:: Livro
c:: Livro
d:: Livro
e:: Livro

a = "A culpa e das Estrelas"
b = "A maça no Escuro"
c = "As vantagens de Ser Invisvel"
d = "Quincas Borba"
e = "Memorias Postumas de Bras Cubas"


listaEmprestimo:: Emprestimo
listaEmprestimo = [(erivelton,a),(erivelton,b),(sanny,a),(sanny,d),(sanny,e),(lucas,b),(gustavo,c),(gustavo,e),(gustavo,a),(gustavo,b),(leticia,b),(leticia,d)]

consultaEmprestimoPessoa:: Pessoa -> [Livro]
consultaEmprestimoPessoa pessoa = [ snd(livros) | livros<-listaEmprestimo,  fst livros == pessoa]

{- consultaEmprestimo:: Pessoa -> Emprestimo
consultaEmprestimo pessoa = [ livros | livros<-listaEmprestimo,  fst livros == pessoa]-}

consultaEmprestimoLivro:: Livro -> [Pessoa]
consultaEmprestimoLivro livro = [ fst(pessoa) | pessoa<-listaEmprestimo, snd pessoa == livro]

emprestado:: Livro -> Bool
emprestado livro =  head[True | x<-listaEmprestimo, livro == snd x]

qntLivros:: Pessoa -> Int
qntLivros pessoa = length [qnt | qnt<-listaEmprestimo, pessoa == fst qnt]

addLista:: (Pessoa, Livro) -> Emprestimo
addLista (pessoa,livro) = (pessoa,livro):listaEmprestimo

removeLista:: (Pessoa,Livro) -> Emprestimo
removeLista (pessoa,livro) = [(x,y) | (x,y)<-listaEmprestimo, (x,y) /= (pessoa,livro)]

-- MESMAS QUESTÕES COM RECURSÃO
consultaPessoa:: Pessoa -> Emprestimo -> [Livro]
consultaPessoa p [] = []
consultaPessoa pessoa ((x,y):xs) | pessoa == x = y : consultaPessoa pessoa xs
                                 | otherwise = consultaPessoa pessoa xs

consultaLivro :: Livro -> Emprestimo -> [Pessoa]
consultaLivro l [] = []
consultaLivro livro ((x,y):xs) | livro == y = x : consultaLivro livro xs    
                               | otherwise = consultaLivro livro xs

isEmprestado :: Livro -> Emprestimo -> Bool
isEmprestado l [] = False
isEmprestado livro ((x,y):xs) | livro == y = True
                              | otherwise = isEmprestado livro xs

quantLivro :: Pessoa -> Emprestimo -> Int
quantLivro p [] = 0
quantLivro pessoa ((x,y):xs) | pessoa == x =  1 + quantLivro pessoa xs
                             | otherwise = quantLivro pessoa xs

remover :: (Pessoa, Livro) -> Emprestimo -> Emprestimo
remover (x,y) [] = []
remover (pessoa,livro) ((x,y):xs) | (pessoa,livro) == (x,y) = remover (pessoa,livro) ((x,y):xs)
                                 | otherwise = (pessoa,livro) : remover (pessoa,livro) ((x,y):xs)

elemNum :: [Int] -> Int -> Int
elemNum [] _ = 0
elemNum (x:xs) num | (num == x) = 1 + elemNum xs (num)
                   | otherwise = elemNum xs (num)

unicos :: [Int] -> [Int]
unicos [] = []
unicos (x:xs) | elemNum xs x >= 1 = unicos listaFiltrada
              | otherwise = x : unicos listaFiltrada
               where listaFiltrada = remove x xs

remove :: Int -> [Int] -> [Int]
remove _ [] = []
remove num (x:xs) | num == x = remove num xs
                  | otherwise = x : remove num xs

-- end..


-- SLIDE 9 RECURSAO GERAL
insOrd :: Int -> [Int] -> [Int]
insOrd y [] = [y] --caso base
insOrd y (z:zs) --caso geral
 | y <= z = y : z : zs
 | otherwise = z : insOrd y zs

ordInsercao :: [Int] -> [Int]
ordInsercao [] = [] --caso base
ordInsercao (x:xs) = insOrd x (ordInsercao xs) 


intercala :: [Int] -> [Int] -> [Int]
intercala xs [] = xs --casos base
intercala [] ys = ys
intercala (x:xs) (y:ys) --caso geral
 | x <= y = x: intercala xs (y:ys)
 | otherwise = y : intercala (x:xs) ys

mergeSort :: [Int] -> [Int]
mergeSort [] = [] --caso base
mergeSort [x] = [x]
mergeSort xs = intercala (mergeSort as) (mergeSort vs)
                 where meio = (length xs) `div` 2
                       as = take meio xs
                       vs = drop meio xs
 
insertOrd :: Int -> [Int] -> [Int]
insertOrd y [] = [y]
insertOrd x (y:ys) | x <= y = x : (y:ys)
                   | otherwise = y : insertOrd x ys

ordInsert :: [Int] -> [Int]
ordInsert [] = []
ordInsert (x:xs) = insertOrd x (ordInsert xs)

iguais :: Int -> [Int] -> Bool
iguais _ [] = False
iguais x (y:ys) | x == y = True 
                | otherwise = iguais x ys

listaDisjunta :: [Int] -> [Int] -> Bool
listaDisjunta [] [] = True
listaDisjunta (x:xs) (y:ys) | iguais x ys || iguais y xs = False
                            | otherwise =  listaDisjunta xs ys

divMod1::Int->Int->(Int,Int)                 
divMod1 a 1 = (a, 0)
divMod1 0 b = (0, 0)
divMod1 a b = if a >= b then (1+cocienteAmenosB , restoAmenosB) else (0, a) 
  where cocienteAmenosB = fst(divMod1 (a-b) b)
        restoAmenosB = snd(divMod1 (a-b) b)   

