-- 1.a Uma função que determina se os quatro inteiros são iguais.
quatroIguais:: Int -> Int -> Int -> Int -> Bool
quatroIguais a b c d = if (a==d) && (a==b) && (a==c) then True else False

-- 1.b Usando a função tresIguais, vista em sala de aula, refaça o item (a).
quatroIguais2:: Int -> Int -> Int -> Int -> Bool
quatroIguais2 a b c d = (a==b) && (b==c) && (c==d)

-- 1.c Usando definições locais let e depois where refaça o item (a).
-- Usando let () in ()
quatroIguais3:: Int -> Int -> Int -> Int -> Bool
quatroIguais3 a b c d = let iguais1 = (a==b); iguais2 = (a==c); iguais3 = (a==d) in 
                           if iguais1 && iguais2 && iguais3 then True else False

-- Usando where
quatroIguais4:: Int -> Int -> Int -> Int -> Bool
quatroIguais4 a b c d = if iguais1 && iguais2 && iguais3 then True else False
                        where iguais1 = (a==b)
                              iguais2 = (a==c)
                              iguais3 = (a==d)

-- 2.a Uma função para saber se os três são diferentes.
tresDiferentes:: Int -> Int -> Int -> Bool
tresDiferentes a b c | (a /= b) && (b /= c) && (c /= a) = True
                     | otherwise = False

-- 2.b Usando (a) e a função tresIguais elabore uma função para saber quantos são iguais.
qntIguais:: Int -> Int -> Int -> Int
qntIguais a b c | tresDiferentes a b c = 0
                | tresIguais a b c = 3
                | otherwise = 2
                where tresIguais a b c = (a==b) && (b==c) 

-- 2.c Usando (b) elabore uma função para saber, dados quatro inteiros, quantos são iguais      
qntIguais2:: Int -> Int -> Int -> Int -> Int
qntIguais2 a b c d =  if (d==b && d==a) || (d==b && d==c) || (d==c && d==a)
                      then (qntIguais a b c + 1) else qntIguais a b c      

{-
qntIguais3:: Int -> Int -> Int -> Int -> Int
qntIguais3 a b c d = (if a==b then 1 else 0) + (if a==c then 1 else 0) + (if a==d then 1 else 0) +
                    (if c==b then 1 else 0) + (if d==b then 1 else 0) + (if d==c then 1 else 0)
                 -}

     
