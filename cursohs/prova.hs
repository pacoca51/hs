doubleAll :: [Int] -> [Int]
f n
    | n < 0 = 0
    | n == 0 = 1
    | otherwise = n * f (n-1)

doubleAll [ ] = [ ]
doubleAll (x : xs) = 2*x : doubleAll xs

roundAll :: [Float] -> [Int]
roundAll [ ] = [ ]
roundAll (f : fs) = round f : roundAll fs

map' :: (a -> b) -> [a] -> [b]
map' f [ ] = [ ]
map' f (x:xs) = f x : map' f xs

impar x = mod x 2 == 1

filter' p [ ] = [ ]
filter' p (x:xs)
    | p x = x : filter p xs
    | otherwise = filter p xs

zipWith' f (x:xs) (y:ys) = f x y : zipWith f xs ys
zipWith' f _ _ = [ ]