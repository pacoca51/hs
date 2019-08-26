
-- stack --resolver lts-13.7 script
doubleSmallNumber x = if x > 100 then x else  x*2

--Operador ++ para juntar duas listas, Ex.: [1,2,3,4] ++ [5,6,7,8] vai gerar [1,2,3,4,5,6,7,8]
--Com strings: "hello" ++ " " + "wolrd" vai gerar "hello world"
--Ainda com strings: ['w','o'] ++ ['o','t'] vai gerar "woot"


--Adicionando um elemento ao inicio de uma lista utiliza o ":"
--Ex.: 5:[1,2,3,4,] vai gerar [5,1,2,3,4]
--Ex.: 'Q':" gatinha" vai gerar "Q gatinha"

--Para saber o elemento de um índice utiliza o operador "!!"
--Ex.: [1,2,3] !! 0 vai gerar 1

--Palavra chave let define o nome correto da lista para o GHCi
--Ex.: let b = [[1,2,3],[4,5,6],[7,8,9]]


--Listas não podem ser de tipos diferentes


--head recebe uma lista e retorna o seu head. O head (cabeça) de uma lista é basicamente o primeiro elemento.
--Ex.: head [5,4,3,2,1]  vai gerar 5


--tail recebe uma lista e retorna a sua "cauda". Em outras palavras, ele decepa a cabeça de uma lista e retorna a cauda.
--Ex.: tail [5,4,3,2,1] vai gerar [4,3,2,1]   


--last recebe uma lista e retorna o seu último elemento.
--Ex.: last [5,4,3,2,1] vai gerar 1


--init recebe uma lista e retorna tudo com exceção do último elemento.
--Ex.: init [5,4,3,2,1]  vai gerar [5,4,3,2]   

--length recebe uma lista e retorna o seu length (tamanho), obviamente.
--Ex.: length [5,4,3,2,1] vai gerar 5


--null verifica se a lista é vazia. Se for, então retorna True, senão retorna False. Utilize esta função no lugar de xs == [] (Caso você tiver uma lista chamada xs)
--Ex.: null [1,2,3]  vai gerar False  
--Ex.: null []  vai gerar True


--reverse reverte uma lista.
--Ex.: reverse [5,4,3,2,1]  vai gerar [1,2,3,4,5]  


--take recebe um número e uma lista. Ele extrai a quantidade de elementos desde o início da lista.. Observe.
--Ex.: take 3 [5,4,3,2,1]  vai gerar [5,4,3]  
--Ex.: take 1 [3,9,3]  vai gerar [3]  
--Ex.: take 5 [1,2]  vai gerar [1,2]  
--Ex.: take 0 [6,6,6]  vai gerar []  
--Observe que se tentarmos tirar mais elementos do que há na lista, ele retorna só a lista. Se nós tentarmos retornar 0 elementos, receberemos uma lista vazia.


--drop funciona de forma similar, só que retira o número de elementos a partir do ínicio da lista.
--Ex.: drop 3 [8,4,2,1,5,6] vai gerar [1,5,6]  
--Ex.: drop 1 [7,6,5,4] vai gerar [6,5,4]  
--Ex.: drop 0 [1,2,3,4]  vai gerar [1,2,3,4]  
--Ex.: drop 100 [1,2,3,4] vai gerar []


--maximum recebe uma lista de coisas que podem ser colocadas em algum tipo de ordem e retorna o seu maior elemento.
--minimum retorna o menor.

--Ex.: minimum [8,4,2,1,5,6] vai gerar 1  
--Ex.: maximum [1,9,2,3,4] vai gerar 9


--sum recebe uma lista de números e retorna a sua soma.
--product recebe uma lista de números e retorna o seu produto.

--Ex.: sum [5,2,1,6,3,2,5,7] vai gerar 31  
--Ex.: product [6,2,1,2] vai gerar 24  
--Ex.: product [1,2,5,6,7,9,2,0] vai gerar 0


--elem recebe alguma coisa e uma lista de coisas e nos diz se esta coisa é um elemento da lista. Geralmente é chamado como uma função infixa porque é mais fácil de ler dessa maneira.
--Ex.: 4 `elem` [3,4,5,6] vai gerar True  
--Ex.: 10 `elem` [3,4,5,6] vai gerar False  