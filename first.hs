dobro x = x*2

ouExclusivo x y = (x || y) && not (x && y)

tresIguais x y z = (x == y) && (y == z)

ehMinusculo::Char->Bool
ehMinusculo c = ('a' <= c) && (c <= 'z')

media3 a b c = (a + b + c) / 3