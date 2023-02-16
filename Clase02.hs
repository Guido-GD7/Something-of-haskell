estanRelacionados :: Float -> Float -> Bool -- are x and y related? Given two real numbers, decide if they are related considering the equivalence relation on R whose equivalence classes are (- ∞,3];(3,7];(7,∞)
estanRelacionados x y |  x <= 3 && y <= 3  = True -- dados dos numeros reales, decide si estan relacionados considerando la relacion de equivalencia en R cuyas clases de equivalencia son (- ∞,3];(3,7];(7,∞)                                  
                      |  x > 3 && x <= 7  && y > 3 && y <= 7 = True
                      |  x > 7 && y > 7 = True
                      | otherwise = False


prodInt :: (Float,Float) -> (Float,Float) -> Float --inner product between to vectors (R2)/Producto interno entre dos vectores de R2
prodInt v w = ( (fst v) * (fst w) ) + ( (snd v) * (snd w) )

prodIntt :: (Float,Float) -> (Float,Float) -> Float -- alternative for inner product/Alternativa para el producto interno
prodIntt (a,b) (d,c) = ((a*d) + (b*c))


todoMenor :: (Float,Float) -> (Float,Float) -> Bool -- Given to vectors R2, decide whether ir is true that each coordinate of the first vector is minor than the corresponding coordinate of the second vector
todoMenor v w | ( fst v) < ( fst w ) && ( snd v ) < ( snd w ) = True -- Dado un vector de R2, decidir si es cierto que cada coordenada del primer vector es menor que su correspondiente coordenada del segundo vector
              | otherwise = False


distanciaPuntos :: (Float,Float) -> (Float,Float) -> Float -- distance between points R2/Distancia entre puntos R2
distanciaPuntos v w = sqrt ( ( fst v - fst w)^2 + ( snd v - snd w )^2)


sumaTerna :: (Float,Float,Float) -> Float -- sum of pythagorean triple/Sumatoria de cada elemento de una tupla
sumaTerna (x,y,z) = x + y + z

posicPrimerPar :: (Int,Int,Int) -> Int -- Given a triplet of integers, returns the position of the first even number if there is any. And returns 4 if they are all odds.  
posicPrimerPar (x,y,z) | mod  x 2  == 0 = 1 -- Dada una tupla de enteros, devuelve la primera posicion del primer par si es que hay. Devuelve 4 si son todos impares
                       | mod  y 2  == 0 = 2 
                       | mod  z 2  == 0 = 3 
                       | otherwise = 4


crearPar :: Float -> Float -> (Float,Float) -- create a ordered pair/Crea un par ordenado
crearPar x y = (x,y)

invertir :: (Float,Float) -> (Float,Float) -- inverse pair/Invierte el par
invertir (x,y) = (y,x)
