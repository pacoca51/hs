{-
Usando guardas, defina funções para
– Dados os coeficientes a, b e c de uma equação
de segundo grau
ax2 + bx + c=0
defina duas funções para calcular as raízes menor
e maior. Se a equação não tiver raízes, as funções
deverão retornar error.
-}

funcaoQuadrada a b c | delta >= 0   = (raizMaior, raizMenor)
                     | otherwise   = (-1,-1)
                     where
                     	delta = b^2 - 4*a*c
                        raizMaior = (negate b + sqrt(delta)) / (2*a)
                        raizMenor = (negate b - sqrt(delta)) / (2*a)


{-
funcaoQuadrada a b c | raizMenor /= (-1) && raizMaior /= (-1)  = show raizMaior ++ "," ++ raizMenor ++ " são as raízes"
                     | otherwise = "ERROR, Delta negativo!"
                     where
                     	delta = b^2 - 4*a*c
                        raizMaior = if delta >= 0 then (negate b + sqrt(delta)) / (2*a) else -1
                        raizMenor = if delta >= 0 then (negate b - sqrt(delta)) / (2*a) else -1-}