 {-1 - Defina uma função que recebe uma lista de inteiro e retorna uma lista com o quadrado de cada elemento. Utilize a função map.
 2 - Defina uma função que dada uma lista de inteiros e um número n, retorne o total de elementos de valor superior a n.
 3 - Crie uma função que recebe uma lista de inteiros e retorna uma outra lista com o seus elementos ao dobro.
 4 - Defina uma função ehPar :: Int -> Bool, que recebe um número e retorna se é par ou não. Defina uma função chamada nPares :: [Int] -> [Int], que recebe como entrada uma lista de números e
retorna somente os que são números pares, utilizando a função filter.
 5 - Defina uma função maioresZero :: [Int] -> bool que verifica se todos os itens da lista são maiores que zero.
 6 -  Crie uma função que recebe uma lista de inteiros e retorne se cada elemento é primo ou não. Lembre-se de definir uma função isPrime que recebe um inteiro e retorna 
 um valor booleano indicando se o número é primo ou não
 7 - Defina uma função removeMaiusculos que recebe uma string e retorna outra string sem os caracteres maiúsculos. Entrada: removeMaiusculos “GalileU SantoS”, saída: “alile anto”. Utilize a função filter.-}


-- Questao 1
quad :: Int -> Int
quad n = n*n
quadrado :: [Int] -> [Int]
quadrado lista = map quad lista

-- Questao 2
superior :: [Int] -> Int -> [Int]
superior lista n = filter (>n) lista

-- Questao 3
dobro :: [Int] -> [Int]
dobro lista = map (*2) lista

-- Questao 4
ehPar :: Int -> Bool
ehPar x = mod x 2 == 0

nPares :: [Int] -> [Int]
nPares lista = filter ehPar lista

-- Questao 5
maioresZero :: [Int] -> Bool
maioresZero lista = length (filter (>0) lista) == length lista

-- Questao 6
isPrime :: Int -> Bool
isPrime 1 = True
isPrime x = length[ divisor | divisor<-[1..x], mod x divisor == 0] == 2

primos :: [Int] -> [Bool]
primos lista = map isPrime lista

-- Questao 7
removeMaiusculos :: String -> String 
removeMaiusculos lista = filter ehMin lista

ehMin :: Char -> Bool
ehMin x = if x `elem` ['A'..'Z'] then False else True

-- Questao 8
junta:: [String] -> [Char] -> String
junta str ch = concat(zipWith (aux1) str ch)

auxPOG :: String -> Char -> String
auxPOG lista ch = reverse(ch : reverse lista)

aux1 :: String -> Char -> String
aux1 lista ch = lista ++ [ch]

