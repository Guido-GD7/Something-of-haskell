productoria :: [Int] -> Int -- Returns product of the elements/Devuelve la productoria de los elementos
productoria n | n == [] = 1 
              | otherwise = head n * productoria  ( tail n ) 

sumarN :: Int -> [Int] -> [Int] -- Given a n number and xs list, add n to each xs element/Dado un numero n y una lista xs, sume n a cada elemento de xs
sumarN n l | l == [] = []
           | otherwise = ( n + head l ) : sumarN n ( tail l )

sumarElPrimero :: [Int] -> [Int] -- Given a no-empty xs list, add the first element to each element of xs. Example: [1,2,3] ---> [2,3,4]/Dada una lista xs no vacia, suma el primer elemento a cada elemento de xs
sumarElPrimero l = sumarN ( head l ) l 

longitud :: [Int] -> Int -- Returns the length of the list/Devuelve la longitud de la lista
longitud l | l == [] = 0 
           | otherwise = 1  + longitud ( tail l )

lastt :: [Int] -> Int -- Similar to last function, had to program if we wanted to use it. Returns the last element of the list/ Similar a la funcion last, debiamos programarla para usarla. Devuelve el ultimo elemento de la lista
lastt l | ( longitud l == 1 ) = head l 
       | otherwise = last ( tail l )


sumarElUltimo :: [Int] -> [Int] -- Given a no-empty xs list, add the last element to each element of xs. Example: [1,2,3] ---> [4,5,6]/Dada una lista xs no vacia, suma el ultimo elemento a cada elemento de xs
sumarElUltimo l = sumarN ( lastt l ) l 


pares :: [Int] -> [Int] -- Returns a list with the even elements of the original list/que devuelve una lista con los elementos pares de la lista original
pares l | l == [] = []
        | ( mod ( head l ) 2 ) == 0 = ( head l : pares ( tail l ) ) 
        | otherwise = pares ( tail l )


quitar :: Int -> [Int] -> [Int] -- Removes the first occurrence of the element in the list(if there is/are any)/Elimina la primera aparicion del elemento en la lista(de haerla)
quitar n l | l == [] = []
           | ( n == head l ) = tail l 
           | otherwise = head l : quitar n ( tail l )


quitarTodas :: Int -> [Int] -> [Int] -- Removes all the occurrence of the element in the list(if there is/are any)/Elimina todas las apariciones del elemento en la lista(de haberla)
quitarTodas n l | l == [] = []
                | ( n == head l ) = quitarTodas n ( tail l )
                | otherwise = head l : quitarTodas n ( tail l)


pertenece :: Int -> [Int] -> Bool -- Decides if n belong to the list/Decide si n pertenece a la lista

initt :: [Int] -> [Int] -- Same of "last", had to program it. An default function in haskell/ Similar a "last", tuvimos que programarla. Una funcion de haskell
initt l |  ( longitud l == 1 ) = []
        | otherwise = head l : initt ( tail l )


reverso :: [Int] -> [Int] -- Similar to reverse. Given a list reverso returns the list but reversed. Example:[1,2,3] ---> [3,2,1]/Similar a reverse. Dada una lista reverso devuelve la lista pero invertida
reverso l | l == [] = []
          | otherwise = ( lastt l ) : reverso ( initt l )
          
eliminarRepetidosAlFinal :: [Int] -> [Int] -- Opposite" to eliminarRepetidosAlInicio. Ex: [1,2,3,9,5,7,8,9,12,9] -> [1,2,3,9,5,7,8,2]/El opuesto" a eliminarRepetidosAlInicio
eliminarRepetidosAlFinal l | l == [] = []
                           | ( pertenece ( head l ) ( tail l ) == True ) = eliminarRepetidosAlFinal ( ( head l ) : ( quitarTodas ( head l ) ( tail l ) ) )
                           | otherwise = ( head l ) : eliminarRepetidosAlFinal ( tail l )


eliminarRepetidosAlInicio :: [Int] -> [Int] -- Which leaves the last occurrence in the list of each element, removing additional repetitions/Deja la ultima aparicion de cada elemento en la lista, eliminando las repeticiones
eliminarRepetidosAlInicio l | l == [] = []
                            | otherwise = reverso ( eliminarRepetidosAlFinal ( reverso l ) ) -- Its missing eliminarRepetidosAlFinal.Opposite of eliminarRepetidosAlInicio/Falta eliminarRepetidosAlFinal.El opuesto a eliminarRepetidosAlInicio
                            

maximo :: [Int] -> Int -- Calculate the max element of a no-empty list/Calcula el elemento maximo de una lista no vacia.
maximo l | ( longitud l == 1 ) = ( head l )
         | ( ( head l ) > head ( tail l ) ) =  maximo ( ( head l ) : quitarTodas ( head ( tail l ) ) ( tail l ) ) 
         | otherwise = maximo ( tail l )



minimo :: [Int] -> Int -- Calculate the min element of a no-empty list/Calcula el elemento minimo de una lista no vacia
minimo l | ( longitud l == 1 ) = ( head l )
         | ( ( head l ) < head ( tail l ) ) =  minimo ( ( head l ) : quitarTodas ( head ( tail l ) ) ( tail l ) ) 
         | otherwise = minimo ( tail l )


ordenar :: [Int] -> [Int] -- Sort items at an increasing order/Ordena los elementos en forma creciente
ordenar l | l == [] = []
          | otherwise =  ( minimo l ) : ordenar ( quitarTodas ( minimo l ) l ) 


