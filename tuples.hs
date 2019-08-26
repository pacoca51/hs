menorMaior:: Int -> Int -> (Int,Int)
menorMaior x y | x <= y = (x,y)
               | otherwise = (y,x)

soma2:: (Int,Int) -> Int
soma2 (x,y) = x + y

{-COM CASAMENTO DE PADRÃO PODERIA ESCREVER
soma2:: (Int,Int) -> Int
soma2 (0, y) = y
soma2 (x,y) = x + y-}

nome:: (String, (Int, Float)) -> String
nome (n, p) = n

-- COLOCA O UNDERSCORE QUANDO Ñ INTERESSA UMA PARTE DO PADRÃO
nome2:: (String, (Int, Float)) -> String
nome2 (n, _) = n

idade:: (String, (Int, Float)) -> Int
idade (_, (id,_)) = id

peso:: (String, (Int, Float)) -> Float
peso (_, (_,p)) = p

-- USANDO CASAMENTO DE PADROES EM TUPLAS NO "WHERE"
imc:: Float -> Float -> String
imc pes alt | res < magro = "Abaixo do peso"
            | res < normal = "Peso normal"
            | res < gordo = "Sobrepeso"
            | otherwise = "Obesidade"
             where res = pes / (alt^2)
                   (magro, normal, gordo) = (18.5,25.0,30.0)


-- Função para pares(x,y) tem a 'fst' que devolve o primeiro e 'snd' que devolve o segundo
somaPar:: (Int, Int) -> Int
somaPar x = fst x + snd x

dobroSomaPar:: (Int, Int) -> Int
dobroSomaPar x = fst x*2 + somaPar x + snd x*2

-- Ex.: Dado um par de inteiros devolver o máximo e quantas vezes ele ocorre
maxOcorre:: Int -> Int -> (Int,Int)
maxOcorre x y | x == y  = (x, 2)
              | otherwise = (max x y, 1)
-- USANDO O fst E O snd (*duvida)
{- maxOcorre:: (Int,Int) -> (Int,Int)
maxOcorre x | fst x == snd x  = (x, 2)
            | otherwise = (max fst x snd x, 1)
-}


-- Ex.: Usando maxOcorre defina uma função similar só que agora para três números.
maxOcorre2:: Int -> Int -> Int -> (Int, Int)
maxOcorre2 a b c = (maior, (if a == maior then 1 else 0) + (if b == maior then 1 else 0) +
                    (if c == maior then 1 else 0))
                 where maior | a >= b && a >= c = a
                             | b >= c = b
                             | otherwise = c



{- Ex.: Dada uma tupla com três elementos inteiros, elabore
uma função para devolver uma tupla com os elementos
em ordem.-}
ordem:: (Int, Int, Int) -> (Int, Int, Int)
ordem (a, b, c) = if (a <= b) && (b <= c) then (a,b,c) else 
	               if (a <= c) && (c <= b) then (a,c,b) else
	               	if (b <= a) && (a <= c) then (b,a,c) else
	               	 if (b <= c) && (c <= a) then (b,c,a) else
	               	  if (c <= a) && (a <= b) then (c,a,b) else
	               	   if (c <= b) && (b <= a) then (c,b,a) else (a,b,c)














