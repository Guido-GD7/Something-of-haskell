multiploDe3 :: Int -> Bool -- Write a function to determine if a natural number is a multiple of 3. It is not allowed to use mod or div
multiploDe3 n | n == 3 = True -- Escribe una funcion que determine si un numero natural es multiplo de 3. No se puede usar mod o div
              | n <= 0 = undefined 
              | n < 3 = False
              | otherwise = multiploDe3 ( n - 3 )

esImpar :: Int -> Bool -- Determinate if a integer numbre is odd/Determina si un entero es impar
esImpar n | n == 1 = True 
          | n == 0 = False
          | otherwise = esImpar ( n - 2 )

sumaImpares :: Int -> Int  -- Given a natural number sum the first n odd numbers. example : n=10 the output will be 1 + 3 + 5 + 7 + 9 = 25 / Dado un numero natural suma los primeros n numeros pares
sumaImpares n | n == 1 = 1 
              | otherwise = ( 2*n - 1) + sumaImpares ( n - 1 )

medioFact :: Int -> Int -- Given a natural number mediofact calculate n!! , n(n-2)(n-4)....
medioFact n | n == 1 = 1 
            | n == 0 = 1 
            | otherwise = n * medioFact ( n - 2)

digitoUnidades :: Int -> Int -- used in sumaDigitos function/utilizado en SumaDigitos
digitoUnidades n | n < 10 = n 
                 | n >= 10 = mod n 10

digitoDecenas :: Int -> Int -- used in sumaDigitos function/utilizado en SumaDigitos
digitoDecenas n = div ( mod n 100 ) 10 

sumaDigitos :: Int -> Int -- Write a function that determines the sum of digits of a positive number. For this function can use div and mod/ Escribe una funcion que determina la suma de los digitos de un numero positivo.Se puede usar div y mod
sumaDigitos n | n < 10 = n 
              | otherwise = digitoUnidades ( n ) + sumaDigitos ( div n 10 )

digitosIguales :: Int -> Bool -- Function that determines if all the digits of a number are equal/Funcion que determina si todos los digitos de un numero son iguales.
digitosIguales n | n < 10 = True 
                 | ( ( mod n 10 ) /= ( digitoDecenas n ) ) = False
                 | ( ( mod n 10 ) == ( digitoDecenas n ) ) = digitosIguales ( div n 10 )




