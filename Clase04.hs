g1 :: Int -> Int -> Int --- Sum from j=i to n of i^j/Sumatoria desde j=i hasta n de i^j
g1 i n | i > n = 0
       | i == 0 = 0
       | n == i = i^n
       | otherwise = i^n + g1 i ( n - 1 )



g22 :: Int -> Int -> Int 
g22 i n | i == 0 = 0 
        | otherwise = ( g22 ( i-1 ) n + i^n ) 
 
g2 :: Int -> Int -- sum from i=1 to n of (sum from j=i to n ) for i^j/Sumatoria desde i=1 hasta n de la sumatoria desde j=i hasta n
g2 n | n == 0 = 0
     | otherwise = g2 ( n - 1 ) + g22 n n


g3 :: Int -> Int -- sum from i=1 to n of 2^i. i=even/Sumatoria de i=1 hasta n de 2^i
g3 n | n == 0 = 0 
     | ( ( mod n 2 ) /= 0 ) = g3 ( n-1 )
     | ( ( mod n 2 ) == 0 ) = g3 ( n-2 ) + 2^n

digitosIguales :: Int -> Bool 
digitosIguales n | n < 10 = True 
                 | ( ( mod n 10 ) /= ( digitoDecenas n ) ) = False
                 | ( ( mod n 10 ) == ( digitoDecenas n ) ) = digitosIguales ( div n 10 )



digitoDecenas :: Int -> Int 
digitoDecenas n = div ( mod n 100 ) 10 



nIguales :: Int -> Int -- Given a n number, sum all the natural numbers minor or equal that n wich have the same digits/Dado un numero n, suma todos los numeros naturales menores o iguales a n que tengan todos los digitos iguales
nIguales n | n == 0 = 0 
           | ( digitosIguales n == True ) = n + nIguales ( n - 1 )
           | ( digitosIguales n == False ) = nIguales ( n-1 )
