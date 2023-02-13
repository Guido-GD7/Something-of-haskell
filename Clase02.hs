estanRelacionados :: Float -> Float -> Bool -- are x and y related? 
estanRelacionados x y |  x <= 3 && y <= 3  = True
                      |  x > 3 && x <= 7  && y > 3 && y <= 7 = True
                      |  x > 7 && y > 7 = True
                      | otherwise = False


prodInt :: (Float,Float) -> (Float,Float) -> Float --inner product
prodInt v w = ( (fst v) * (fst w) ) + ( (snd v) * (snd w) )

prodIntt :: (Float,Float) -> (Float,Float) -> Float -- alternative for inner product
prodIntt (a,b) (d,c) = ((a*d) + (b*c))


todoMenor :: (Float,Float) -> (Float,Float) -> Bool -- analise if the elements of v are minor of the elements of w, understandig that the comparasion are related to the order between parentheses
todoMenor v w | ( fst v) < ( fst w ) && ( snd v ) < ( snd w ) = True 
              | otherwise = False


distanciaPuntos :: (Float,Float) -> (Float,Float) -> Float -- distance between points
distanciaPuntos v w = sqrt ( ( fst v - fst w)^2 + ( snd v - snd w )^2)


sumaTerna :: (Float,Float,Float) -> Float -- sum of pythagorean triple
sumaTerna (x,y,z) = x + y + z

posicPrimerPar :: (Int,Int,Int) -> Int -- position of the first even 
posicPrimerPar (x,y,z) | mod  x 2  == 0 = 1
                       | mod  y 2  == 0 = 2 
                       | mod  z 2  == 0 = 3 
                       | otherwise = 4


crearPar :: Float -> Float -> (Float,Float) -- create a ordered pair 
crearPar x y = (x,y)

invertir :: (Float,Float) -> (Float,Float) -- inverse pair
invertir (x,y) = (y,x)