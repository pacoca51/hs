fat n
    | n < 0 = 0
    | n == 0 = 1
    | otherwise = n * fat (n-1)

anp n p = (fat n) / (fat (n-p))

com n p = (fat n) / (fat p*fat (n-p))