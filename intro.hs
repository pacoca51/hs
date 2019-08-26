-- stack --resolver lts-13.7 script
m = putStrLn "Hello World"

--Funcão soma de todos os numeros (4 = 1 + 2 + 3 + 4 = 10)
soma 1 = 1
soma n = n + soma(n-1)

--Função dobro
doubleMe x = x + x

-- doubleUs x y = x + x + y + y--
doubleUs t y = doubleMe t + doubleMe y

-- Função fatorial
fat 0 = 1
fat n = n * f (n-1)

-- Fibonacci
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n - 2)

-- Guarda (como se fosse o 'caso' no Java)
guarda a | (a == 0) = 0
  		 | (a == 1) = 1
  		 | otherwise = 10 

-- FAZENDO UMA FUNÇÃO COM VARIAVEL ANONIMA (_)
anon :: Bool -> Bool -> Bool
anon False _ = False
anon _ False = False
anon True True = True