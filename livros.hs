{-Dadas duas listas de palavras, sendo uma de adjetivos e outra de
nomes, construa uma lista que explora todas as combinações de
adjetivos e nomes das duas listas. Ex: adjetivos = [“bonito”, “alegre”],
nomes=[“menino”, “garoto”]. A resposta deve ser [“menino bonito”,
“menino alegre”, “garoto bonito”, “garoto alegre”]-}

--combinacao:: [String] -> [String]
--combinacao adj nome = [lista | lista<-nome

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