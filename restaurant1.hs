type Codigo = Int
type Nome = String
type Preco = Int --em centavos
type ItemRest = (Codigo, Nome, Preco)
type Menu = [ItemRest]
type Categoria = String
type DiaSemana = Int
type Mesa = Int
type Quant = Int
type ItemCliente = (Codigo, Quant)
type PedidoCliente = [ItemCliente]
type PedidosMesas = [PedidoCliente]


cardapio :: Menu
cardapio = [(1,"ESPUMANTE SALTON DEMI-SEC",3000), (50,"VENTISQUEIRO CLASSICO MERLOT",4000), 
            (51,"Ensopadinho de Lagosta",2450), (100,"Batata Rustica Trufada",1560), 
            (101,"Filet A Cavalo",5660), (120,"Filet A Francesa",5750),
            (121,"Frango A Cubana",3400), (140,"Frango A Parmegiana",3550),
            (141,"Frutos do Mar A Italiana",4600), (160,"Peixe A Belle Meuniere",4500),
            (161,"Espaguete A Carbonara",3590), (180,"Talharim A Bolonhesa",3590),
            (181,"Porcao Batata Frita",1099), (200,"Porcao Legumes Grelhados",1099),
            (201,"Petit Gateau de Amendoas",2899), (220,"Torta Pave Chocolate Trufada",2999)]

pedidosRest :: PedidosMesas
pedidosRest = [[(120, 1), (100, 3), (121, 2), (201, 1), (50, 1)], [(1, 1), (201, 1), (161, 2)], [], 
               [(220, 3), (181, 2), (51, 3)], [(160, 2), (140, 3), (200, 2)], [(220, 3), (1, 5), (180, 5), (200, 5)], []]

-- 1.a
coletaItemMenu :: Menu -> Codigo -> ItemRest
coletaItemMenu cardapio cod = head(filter codigo cardapio) where codigo (x,y,z) = cod == x

-- 1.b
atualizaPrecosMenu :: Menu -> Int -> Menu
atualizaPrecosMenu cardapio aumentoPerc = map aumento cardapio where aumento (x,y,z) = (x,y,z+(perc z))
                                                                     perc z = (div (aumentoPerc*z) 100)


--1.c
atualizaPrecosCat :: Menu -> Categoria -> Int -> Menu
atualizaPrecosCat cardapio categoria aumentoPerc =  map aumento cardapio 
                                                     where codigo = (catMaiorMenorCod categoria)
                                                           aumento (x,y,z) | (x >= fst codigo) && (x <= snd codigo) = (x,y,z+(perc z))
                                                                           | otherwise = (x,y,z)
                                                           perc c = (div (aumentoPerc*c) 100)

catMaiorMenorCod :: Categoria -> (Codigo,Codigo)
catMaiorMenorCod categoria | categoria == "Bebidas" = (1,50)
                           | categoria == "Tira-gostos" = (51,100)
                           | categoria == "Carnes" = (101,120)
                           | categoria == "Aves" = (121,140)
                           | categoria == "Peixes e Mariscos" = (141,160)
                           | categoria == "Massas" = (161,180)
                           | categoria == "Acompanhamentos extras" = (181,200)
                           | categoria == "Sobremesas" = (201,220)


-- 2.a
adicionaPedido :: Mesa -> ItemCliente -> PedidosMesas -> PedidosMesas
adicionaPedido mesa (cod,qnt) pedidos = take (mesa-1) pedidos ++ map novoItem [(pedidos !! (mesa-1))] ++ drop (mesa) pedidos
                                        where novoItem xs | listaFiltrada == [] = (cod,qnt):xs
                                                          | otherwise = map novoSubItem xs 
                                                          where listaFiltrada = filter (\(x,y) -> x == cod) xs   
                                              novoSubItem (x,y) | x /= cod = (x,y)
                                                                | otherwise = (cod,y+qnt) 
-- 2.b
cancelaPedido :: Mesa -> ItemCliente -> PedidosMesas -> PedidosMesas
cancelaPedido mesa (cod,qnt) pedidos =  take (mesa-1) pedidos ++ listaMesaAtualizada ++ drop (mesa) pedidos
                                        where novoItem (x,y) | (x == cod && qnt >= y) = []
                                                             | (x == cod && qnt < y) = [(x,y-qnt)]
                                                             | otherwise = [(x,y)]
                                              listaMesa = (pedidos !! (mesa-1))
                                              listaMesaAtualizada = [concat(map novoItem listaMesa)]
--2.c
pedidoOrdenado :: Mesa -> PedidosMesas -> PedidoCliente --  ou PedidosMesas? Pois é apenas para uma mesa...
pedidoOrdenado mesa pedidos = quicksort (pedidos !! (mesa-1))
                                   
-- FUNÇÃO PARA ORDENAR
quicksort :: [(Int,Int)] -> [(Int,Int)]
quicksort [] = []
quicksort ((cod,qnt):xs) = quicksort (filter (\(a,b) -> a<cod) xs) ++ [(cod,qnt)] ++ quicksort (filter (\(a,b) -> a>=cod) xs)

--2.d
pedidoCompletoMesa :: Mesa -> PedidosMesas -> Menu -> [(Quant,Nome,Preco)]
pedidoCompletoMesa mesa pedidos cardapio = map atualizaItem listaMesa
                         where listaMesa = (pedidoOrdenado mesa pedidos)
                               atualizaItem (cod,qnt) = head [ (qnt,nome,prec*qnt) | (codItem,nome,prec)<-cardapio, cod==codItem]

--3.a
desconto :: [(Quant,Nome,Preco)] -> Menu -> DiaSemana -> Int -> [(Quant,Nome,Preco)]
desconto pedidoCompMesa cardapio dia desc = map verifica pedido
                    where pedido = concat [[ (cod,nome,prec,qnt) | (qnt,nome,prec)<-pedidoCompMesa, nome == nomeItem ] | (cod,nomeItem,precItem)<-cardapio]
                          verifica (x,y,z,a) | ((x >= (fst codDesc)) && (x <= (snd codDesc))) = (a,y,(z-descItem))
                                             | otherwise = (a,y,z)
                                              where descItem = div (desc*z) 100
                          codDesc = descontoDiaCategoria dia

descontoDiaCategoria :: DiaSemana -> (Codigo,Codigo)
descontoDiaCategoria dia | dia == 2 = (1,50)
                         | dia == 3 = (141,160)
                         | dia == 4 = (51,100)
                         | dia == 5 = (101,140)
                         | dia == 6 = (161,180)
                         | ((dia == 1) || (dia == 7)) = (181,220)

--4.a
totalMesa :: [(Quant, Nome, Preco)] -> Preco
totalMesa pedComMesa = sum (map (\(qnt,nome,prec) -> prec) pedComMesa)

--4.b
formataLinhas :: [(Quant,Nome,Preco)] -> String
formataLinhas pedidos = concat(map formataLinha pedidos)

formataPreco:: Preco->String
formataPreco i = t (show((read (show (i) ++ ".0"))/100))

t::String->String
t i = if length(dropWhile (/='.') i) == 2 then finalZero else finalDiferente 
      where finalZero  | length i == 3 = "..." ++ i ++ "0"
                       | length i == 4 = ".." ++ i ++ "0"
                       | length i == 5 = "." ++ i ++ "0"
                       | length i == 6 = i ++ "0"
                       | otherwise = i
            finalDiferente | length i == 3 = "...." ++ i 
                           | length i == 4 = "..." ++ i 
                           | length i == 5 = ".." ++ i 
                           | length i == 6 = "." ++ i 
                           | otherwise = i

formataLinha::(Quant,Nome,Preco)-> String
formataLinha (x,y,z) | x >= 100 = (show x ++ " " ++ y ++ te y ++ formataPreco z ++"\n")
                     | x >= 10 = (show x ++ "  " ++ y ++ te y ++ formataPreco z ++"\n")
                     | otherwise = (show x ++ "   " ++ y ++ te y ++ formataPreco z ++"\n")

te:: String->String
te a = concat["."| x<- [1..(30-length a)]]


{- 5 QUESTAO
Definições
desconto _ [] = []  -- c1
desconto y (x:xs)= div(y*x) 100 : desconto y xs -- c2
sum [] = 0 -- c3
sum x:xs = x + sum xs -- c4
[] ++ xs = xs -- c5
(x:xs) ++ zs = x:(xs++zs) -- c6

--letra (a) - sum (desconto y xs) = div (y* sum xs) 100
Caso Base:
-- caso base para lado esquerdo
sum(desconto y []) 
sum ([]) = 0 -- usando c3
-- caso base para lado direito 
div(y*sum []) 100 
div (y*0) 100
div 0 100 = 0

Hipótese Indução : O teorema vale para uma lista de n elementos 

Caso Geral:
lado esquerdo: 
  sum (desconto y x:xs) 
Usando c2 temos:
  sum (div (y*x)100 : desconto y xs)
Usando c4 temos:
  div (y*x)100 + sum (desconto y xs)
Usando a definição temos:
  div (y*x)100 + div (y* sum xs) 100 -- final lado esquerdo

lado direito:
   div (y* sum (x:xs)) 100 
Usando c4 temos:
   div (y* (x + sum xs)) 100
Fazendo álgebra 
   div (y*x)100 + div (y* sum xs) 100 -- final do lado direito 

#Tanto o final do lado direito quanto o final do lado esquerdo são iguais, provando assim que a tese é verdadeira

-- letra (b)
sum ((desconto y xs) ++ (desconto y zs)) = sum (desconto y xs) + sum (desconto y zs)

Caso Base:
-- caso base lado esquerdo
sum ((desconto y []) ++ (desconto y zs)) 
sum ([] ++ (desconto y zs)) -- usando c5
sum (desconto y zs)

-- caso base lado direito
sum (desconto y []) + sum (desconto y zs) 
sum ([]) + sum (desconto y zs)
0 + sum (desconto y zs)
sum (desconto y zs)

Hipótese Indução : O teorema vale para uma lista de n elementos 

Caso Geral:
lado esquerdo:
    sum ((desconto y x:xs) ++ (desconto y zs))
Usando c2 temos:
    sum ((div (y*x)100 : desconto y xs) ++ (desconto y zs))
Usando c6 temos:
    sum ((div (y*x)100 : (desconto y xs ++ desconto y zs)) 
Usando c4 temos:
    div (y*x)100 + sum ((desconto y xs) ++ (desconto y zs))
Usando a definição temos:
    div (y*x)100 + sum (desconto y xs) + sum(desconto y zs) 

lado direito:
    sum (desconto y x:xs) + sum (desconto y zs) 
Usando c2 temos:
    sum (div (y*x) 100 : desconto y xs) + sum(desconto y zs)
Usano c4 temos:
    div (y*x) 100 + sum (desconto y xs) + sum(desconto y zs) 

#Tanto o final do lado direito quanto o final do lado esquerdo são iguais, provando assim que a tese é verdadeira-}



















