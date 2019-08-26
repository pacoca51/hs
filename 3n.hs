
{-Escreva uma função para dada uma lista de inteiros,
seleciona os ímpares desta lista e devolve uma lista em
que os elementos ímpares aparecem triplicados.-}
imp:: [Int] -> [Int]
imp x = [ 3 * n | n <- x, mod n 2 == 1 ]