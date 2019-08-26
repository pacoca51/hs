
diferente a b c = if ((a /= b) && (b /= c) && (c /= a)) then True else False

menor a b | (a < b)  = a
          | (a > b)  = b
          | otherwise  = a

--menorDeTres a b c = if ((a < b) && (a < c)) then a else
	--           if ((b < a) && (b < c)) then b else
	--           	if ((c < a) && (c < b)) then c else 0 

menorDeTres a b c = if ((a < b) && (a < c)) then a else
	                if (b < c) then b else c
	        
 
menorDeTres2 a b c | a <= b && a <= c = a
                   | b <= c = b
                   | otherwise = c


acimaMedia a b c | (a > media) || (b > media) || (c > media) = 1
                 | ((a > media) && (b > media)) || ((b > media) && (c > media)) || ((a > media) && (c > media)) = 2
                 | otherwise  = 0
                 where media = (a + b + c) / 3
                 

{-acimaMedia2 a b c = if ((a > media) && (b > media)) || ((b > media) && (c > media))
 || ((a > media) && (c > media)) then 2 else 1-}

{- acimaMedia2 a b c | a > media  = soma 1
                   | b > media  = soma 1
                   | c > media  = soma 1
                   | otherwise  = 0
                   where media = (a + b + c) / 3
                   soma 1 = soma 1 + 1

-}

acimaMedia2 a b c = (if (a > media) then 1 else 0) + (if (b > media) then 1 else 0) + (if (c > media) then 1 else 0)
                     where  media = (a + b + c) / 3
