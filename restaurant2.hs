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

cardapio :: Menu
cardapio = [(001, "Pastel", 700), (002, "Agua", 200), (003, "Cerveja", 500),
            (004, "Picanha", 8050), (005, "Pudim", 775),(006, "Batata Frita", 1000), 
            (007, "Lasanha de Frango", 1500), (008, "Torta de Chocolate", 5200), (009, "Sanduiche de Peru", 600), 
            (010, "Guarana 2L", 600), (011, "Coca Cola Lata", 400)]



pedidosRest :: PedidosMesas
pedidosRest = [[(002, 1), (004, 2), (006, 1), (010, 1), (005,2)], [], [], 
               [(007, 1), (003, 3), (002, 2)], [(001, 3), (011, 1)], [(002, 1)], [(008, 1), (010, 3)], [(009, 1), (011, 1)]]


{- 3.1 (a) Adiciona um item no menu. Se o código do item já existir no menu deve 
retornar uma mensagem de erro sinalizado que existe um item já cadastrado para
aquele código.-}

adicionaItemMenu :: Menu -> ItemRest -> Menu
adicionaItemMenu ((a, b, c):xs) (codigo , nome, preco) = if (length listaVerificada == length ((a, b, c):xs)) then (codigo, nome, preco) : listaVerificada 
                                                         else error "CODIGO JA CADASTRADO"
                                                         where listaVerificada = [ (x,y,z) | (x, y, z)<-(a, b, c):xs, codigo /= x]


{- 3.1 (b) Remove um item no menu, informando seu código. Se o código do item não
existir no menu deve retornar uma mensagem de erro sinalizando que não existe
um item no menu para aquele código.-}

removeItemMenu :: Menu -> Codigo -> Menu
removeItemMenu  ((a, b, c):xs) codigo = if (length listaVerificada /= length ((a, b, c):xs)) then  listaVerificada
                                        else error "CODIGO INEXISTENTE NO SISTEMA"
                                        where listaVerificada = [ (x,y,z) | (x, y, z)<-(a, b, c):xs, codigo /= x]


{- 3.1 (c) Coleta um item no menu, informando seu código. Para simplificar, considere
que esta operação só tem o caso de sucesso, ou seja, o item consultado sempre vai
existir no menu.-}

coletaItemMenu :: Menu -> Codigo -> ItemRest
coletaItemMenu ((a, b, c):xs) codigo = head [ (x,y,z) | (x, y, z)<-(a, b, c):xs, codigo == x]


{- 3.2 (a) Adiciona um pedido de uma mesa na lista de pedidos do restaurante. Se a lista
de pedidos da mesa não for vazia, a função precisa checar se o código do item
agora solicitado já existe na lista de pedidos da mesa. Em caso afirmativo, a
quantidade a ele associada vai ser incrementada com a nova solicitação. Caso
contrário, o novo item vai apenas ser adicionado à lista já existente. Esta função
assume, por simplicidade, que o código do item a ser incluído existe no menu do
restaurante.-}


verificaCodIgual :: Mesa -> ItemCliente -> PedidosMesas -> PedidosMesas
verificaCodIgual  mesa (cod,qnt) x = [[ (a,b) | (a,b)<-(x !! (mesa-1)), cod == a]]

calculaAdicionaPedido :: Mesa -> ItemCliente -> PedidosMesas -> PedidosMesas
calculaAdicionaPedido mesa (cod, qnt) x = if  ([] `elem` (verificaCodIgual mesa (cod, qnt) x)) then [concat ([(cod, qnt)]: listaMesa)] else juntaLista
                                          where listaMesa = [x !! (mesa-1)]
                                                juntaLista =  [concat ([[(a,b+qnt)] | [(a,b)]<-verificaCodIgual mesa (cod, qnt) x] ++ [novaLista])]
                                                novaLista = [(a,b) | (a,b)<- head listaMesa, cod/=a]

adicionaPedido :: Mesa -> ItemCliente -> PedidosMesas -> PedidosMesas
adicionaPedido mesa (cod, qnt) x = (take (mesa-1) x) ++ calculaAdicionaPedido mesa (cod,qnt) x ++ (drop mesa x)


{- 3.2 (b) Cancela um pedido na lista de pedidos do restaurante, para uma dada mesa.
Por simplicidade, suponha que o pedido a ser cancelado existe na lista de pedidos
da mesa. No ato do cancelamento, se a quantidade a ser cancelada for igual ou
superior à quantidade já solicitada, o item deve ser removido da lista de pedidos
da mesa. Se o cancelamento for parcial, o item permanecerá na lista, mas com a
quantidade decrementada de acordo com a quantidade cancelada.-}

verificaCodIgual2 :: Mesa -> ItemCliente -> PedidosMesas -> PedidosMesas
verificaCodIgual2  mesa (cod,qnt) x = [[ (a,b) | (a,b)<- (x !! (mesa-1)), cod == a, qnt >= b]]

{-cancelaPedido :: Mesa -> ItemCliente -> PedidosMesas -> PedidosMesas
cancelaPedido mesa (cod, qnt) x = if (null (verificaCodIgual2 mesa (cod, qnt) x)) == False then [novaLista] else juntaLista
                                   where listaMesa = [pedidosRest !! (mesa-1)]
                                         addPedido = [(cod, qnt):(a,b):xs | (a,b):xs<-listaMesa, cod/=a]
                                         juntaLista =  [concat ([[(a,b-qnt)] | [(a,b)]<-verificaCodIgual2 mesa (cod, qnt) x] ++ [novaLista])]
                                         novaLista = [(a,b) | (a,b)<- head listaMesa, cod/=a]-}

calculaCancelaPedido :: Mesa -> ItemCliente -> PedidosMesas -> PedidoCliente
calculaCancelaPedido mesa (cod, qnt) x = if [] `elem` (verificaCodIgual2 mesa (cod, qnt) x) then juntaLista else novaLista 
                                         where listaMesa = [x !! (mesa-1)]
                                               juntaLista =  concat ( [[(a,b-qnt) | (a,b)<- head listaMesa, cod==a]] ++ [novaLista])
                                               novaLista = [(a,b) | (a,b)<- head listaMesa, cod/=a]

cancelaPedido :: Mesa -> ItemCliente -> PedidosMesas -> PedidosMesas
cancelaPedido mesa (cod, qnt) x = (take (mesa-1) x) ++ [calculaCancelaPedido mesa (cod,qnt) x] ++ (drop mesa x)


{- 3.2 (c) Gera lista completa do pedido, que será usada quando a conta for finalizada.
Os nomes e os preços de cada item são coletados do menu, usando o código do 
item para fazer a coleta. O preço gerado na lista de saída já é o preço do item
totalizado, ou seja, o preço unitário multiplicado pela quantidade.-}

pedidoCompletoMesa :: Mesa -> PedidosMesas -> Menu -> [(Quant, Nome, Preco)]
pedidoCompletoMesa mesa x menu =  concat [ [(qnt,nome,preco*qnt) | (codMenu,nome,preco)<-menu, codLista==codMenu] | (codLista,qnt)<-listaMesa] where listaMesa = (x !! (mesa-1)){- -- (a,b)
                                    menu -- (a,b,c) verifica a_menu == a_listaMesa, (b_listaMesa,b_menu,c)
                                    (a_listaMesa, b_menu,c*a_listaMesa)
                                    está fora de ordem pq pega pela ordem do codMenu
 -}


{- 3.2 (d) Gera o total da conta a partir da lista de pedidos de uma mesa, após aplicar a
função do item 3.2 (c).-}

--listaPedidosMesa = pedidoCompletoMesa 2 pedidosRest cardapio

totalMesa :: [(Quant, Nome, Preco)] -> Preco
totalMesa listaPedidosMesa = sum [preco | (qnt,nome,preco)<-listaPedidosMesa]

-- 3.3 formata linhas
--letra a formata preco

formataPreco:: Preco->String
formataPreco i = t (show((read (show (i) ++ ".0"))/100))

t::String->String
t i
 | length i == 3 = "..." ++ i ++ "0"
 | length i == 4 = ".." ++ i ++ "0"
 | length i == 5 = "." ++ i ++ "0"
 | length i == 6 = i ++ "0"
 | otherwise = i

--letras b formata linhas
formataLinha::(Quant,Nome,Preco)-> String
formataLinha (x,y,z) = (show x ++ " " ++ y ++ te y ++formataPreco z ++"\n")
te:: String->String
te a = concat["."| x<- [1..(30-length a)]]

--letra c formata linhas
formataLinhas2::[(Quant,Nome,Preco)] -> String
formataLinhas2 (x:xs) = formataLinha x ++ head[formataLinha x | x<-xs]

-- letra d
formataTotal:: [(Quant,Nome,Preco)] -> String
formataTotal total = "\n " ++ concat(replicate 25 ".") ++ "Total" ++ " " ++formataPreco(totalMesa total)
-- letra e
geraConta:: Mesa -> PedidosMesas -> Menu -> IO ()
geraConta x pedidos menu = putStr((formataLinhas2 (pedidoCompletoMesa x pedidos menu)) ++ formataTotal(pedidoCompletoMesa x pedidos menu) ++ "\n")
-- letra f
liberaMesa:: Mesa -> PedidosMesas -> PedidosMesas
liberaMesa mesa pedidos = (take(mesa-1) pedidos) ++ [[]] ++ (drop mesa pedidos)


--4.1
--a)
adicionaItemMenu2::Menu->ItemRest->Menu
adicionaItemMenu2 [] (a,b,c) = [(a,b,c)]
adicionaItemMenu2 ((cd,nm,pr):xs) (a,b,c) | a == cd = error "já existe um item cadastrado para aquele código"
                                          | otherwise = (cd,nm,pr) : adicionaItemMenu2 xs (a,b,c)  
  

--b)
removeItemMenu2::Menu -> Codigo -> Menu 
removeItemMenu2 [] _ = error "não existe um item no menu para aquele código"
removeItemMenu2 ((cd,nm,pr):xs) i = if cd==i  then xs  else [(cd,nm,pr)] ++ removeItemMenu2 xs i  

--c)
coletaItemMenu2 :: Menu -> Codigo -> ItemRest
coletaItemMenu2 [(x,y,z)] i = (x,y,z)
coletaItemMenu2 ((cd,nm,pr):xs) i = if cd == i then head((cd,nm,pr): [coletaItemMenu2 xs i]) else coletaItemMenu2 xs i 

-- 4.2
--a)
adicionaPedido2 :: Mesa -> ItemCliente -> PedidosMesas -> PedidosMesas
adicionaPedido2 mesa (a,b) pedidos = (take (mesa-1) pedidos) ++ [listaMesa2 (a,b) (pedidos !! (mesa-1))] ++ (drop mesa pedidos)
                                 
listaMesa2 :: ItemCliente -> PedidoCliente -> PedidoCliente
listaMesa2 (cod,qnt) [] = [(cod,qnt)]
listaMesa2 (cod,qnt) ((codigo,quantidade):xs) | cod == codigo = (cod, qnt+quantidade) : xs
                                              | cod /= codigo = (codigo, quantidade) : listaMesa2 (cod,qnt) xs


-- 4.2 (b)
cancelaPedido2 :: Mesa -> ItemCliente -> PedidosMesas -> PedidosMesas 
cancelaPedido2 mesa (a,b) pedidos = (take (mesa-1) pedidos) ++ [listaMesac (a,b) (pedidos !! (mesa-1))] ++ (drop mesa pedidos)

listaMesac :: ItemCliente -> PedidoCliente -> PedidoCliente
listaMesac (cod,qnt) [] = []
listaMesac (cod,qnt) ((codigo,quantidade):xs) | (cod == codigo) && (qnt >= quantidade) =  xs
                                              | (cod == codigo) && (qnt < quantidade) = (cod, quantidade-qnt) : xs
                                              | (cod /= codigo) = (codigo, quantidade) : listaMesac (cod,qnt) xs


-- 4.2 (c)
pedidoCompletoMesa2 :: Mesa -> PedidosMesas -> Menu ->[(Quant, Nome, Preco)]
pedidoCompletoMesa2 1 (pedidos:xs) menu = listaItens pedidos menu
pedidoCompletoMesa2 m (pedidos:xs) menu = pedidoCompletoMesa2 (m-1) xs menu 

buscaItemMesa::ItemCliente->Menu->(Quant,Nome,Preco)
buscaItemMesa (cod,quant) ((a,b,c):xs)
 | cod == a = (quant, b, c*quant)
 | cod /= a = buscaItemMesa (cod,quant) xs
 
listaItens ::PedidoCliente -> Menu ->[(Quant, Nome, Preco)]
listaItens [] xs = []
listaItens (pedidos:ys) menu = (buscaItemMesa pedidos menu) : listaItens ys menu

-- 4.2 (d)
totalMesa2 :: [(Quant, Nome, Preco)] -> Preco 
totalMesa2 [] = 0
totalMesa2 ((a,b,c):xs) =  c + totalMesa2 xs