sumaDivisoresHasta :: Int -> Int -> Int -- Calculate the sum of the divisors of n (integer number) until k(integer number). Ex: If i choose n=6 and k=12, the function returns 12, 1+2+3+6 the divisors of 6 between 1 and 12 where the rest is 0
sumaDivisoresHasta n k | k == 1 = 1     -- Calcula la suma de los divisores de n(numero entero) hasta k(numero entero).Ej: Si elijo n=6 y k=12, la función devuelve 1, 1+2+3+6 que son los divisores de 6 entre 1 y 12 donde el resto es 0
                       | mod n k == 0 = k + sumaDivisoresHasta n (k-1)
                       | mod n k /= 0 = sumaDivisoresHasta n (k-1)
sumaDivisores :: Int -> Int -- Calculate the sum of the divisors of n(integer)/ Calcula la suma de los divisores de un entero positivo
sumaDivisores n | n == 1 = 1
                | otherwise = sumaDivisoresHasta n n 
-- An integer p > 1 is prime if there is no natural k such that 1 < k < p and k divides p/Un entero p > 1 es primo sii no existe un natural k tal que 1 < k < p y k divide a p.

menorDivisorDesde :: Int -> Int -> Int -- Returns the minor divisor of n starting from k. Ex: menorDivisorDesde k=2 n= 6 --> 3 /Devuelve el menor divisor de n empezando desde k
menorDivisorDesde k n  | k == 1 = menorDivisorDesde ( k+1 ) n
                       | k > n = undefined
                       | mod n k == 0 = k 
                       | mod n k /= 0 = menorDivisorDesde ( k+1 ) n

menorDivisor :: Int -> Int -- Returns the minor divisor of n.
menorDivisor n | n == 1 = undefined
               | otherwise = menorDivisorDesde 1 n 


esPrimo :: Int -> Bool -- Define if n its prime/Define si n es primo
esPrimo n | n <= 1 = False
          | menorDivisor n == n = True
          | otherwise = False














