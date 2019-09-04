somar :: (Int,Int) -> (Int,Int) -> (Int,Int)
somar (a,b) (c,d) = (a+c,b+c)

nomes :: (String,String,String)
nomes = ("salve","meu","consagrado")

first (x,_,_) = x
second (_,y,_) = y
third (_,_,z) = z