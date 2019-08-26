type Codigo = Int
type Nome = String
type Preco = Int
type ItemRest = (Codigo,Nome,Preco)
type Menu = [ItemRest]
type Mesa = Int 
type Quant = Int
type ItemCliente = (Codigo,Quant)
type PedidoCliente = [ItemCliente]
type PedidosMesas = [PedidoCliente]
type Categoria = String


cardapio :: Menu
cardapio = [(001, "Pastel", 700), (002, "Agua", 200), (003, "Cerveja", 500),
            (004, "Picanha", 8050), (005, "Pudim", 775),(006, "Batata Frita", 1000), 
            (007, "Lasanha de Frango", 1500), (008, "Torta de Chocolate", 5200), (009, "Sanduiche de Peru", 600), 
            (010, "Guarana 2L", 600), (011, "Coca Cola Lata", 400)]



pedidosRest :: PedidosMesas
pedidosRest = [[(002, 1), (004, 2), (006, 1), (010, 1), (005,2)], [], [], 
               [(007, 1), (003, 3), (002, 2)], [(001, 3), (011, 1)], [(002, 1)], [(008, 1), (010, 3)], [(009, 1), (011, 1)]]


coletaItemMenu :: Menu -> Codigo -> ItemRest
coletaItemMenu cardapio cod = head(filter codigo cardapio) where codigo (x,y,z) = cod == x 

atualizaPrecosMenu :: Menu -> Int -> Menu
atualizaPrecosMenu cardapio aumentoPerc = map aumento cardapio where aumento (x,y,z) = (x,y,z+(perc z))
                                                                     perc z = (div (aumentoPerc*z) 100)


atualizaPrecosCat :: Menu -> Categoria -> Int -> Menu
atualizaPrecosCat cardapio categoria aumento = 


catMaiorMenorCod :: Categoria -> (Codigo,Codigo)
catMaiorMenorCod categoria | categoria == "Bebidas" = (1,50)
                           | categoria == "Tira-gostos" = (51,100)
                           | categoria == "Carnes" = (101,120)
                           | categoria == "Aves" = (121,140)
                           | categoria == "Peixes e Mariscos" = (141,160)
                           | categoria == "Massas" = (161,180)
                           | categoria == "Acompanhamentos extras" = (181,200)
                           | categoria == "Sobremesas" = (201,220)
