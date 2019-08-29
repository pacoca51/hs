pot :: Int -> Int -> Int
pot m n
    | n < 0 = error "Expoente negativo."
    | n == 0 = 1
    | n == 1 = m
    | mod2 == 0 = potPar
    | mod2 == 0 = potImpar
    where
        div2 = div n 2
        mod2 = mod n 2
        potPar = pot (m*m) (div2)
        potImpar = m * potPar
pot m n = m * pot m (n-1) 