sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria l = head(l) + sumatoria(tail(l))

longitud :: [a] -> Int
longitud [] = 0
longitud l = 1+longitud(tail(l))

pertenece :: Int -> [Int] -> Bool
pertenece a [] = False
pertenece a b |a==head(b) = True
              |otherwise = pertenece a (tail(b))

pertenece2 :: Int -> [Int] -> Bool  --COMO LO HICIERON EN LA CLASE TEORICA
pertenece2 x l | l==[] = False
              | otherwise = (x==head(l)) || pertenece2 x (tail(l))

sumatoriaPM :: [Int] -> Int             --SUMATORIA PATTERN MATCHING COMO LO HICIERON EN LA CLASE TEORICA
sumatoriaPM [] = 0
sumatoriaPM (x:xs) = x + sumatoriaPM xs

longitudPM :: [a] -> Int                --LONGITUD PATTERN MATCHING COMO LO HICIERON EN LA CLASE TEORICA
longitudPM [] = 0
longitudPM (_:xs) = 1 + longitudPM xs

productoria :: [Int] -> Int
productoria [] = 1
productoria l = head(l) * productoria(tail l)

sumarN :: Int -> [Int] -> [Int]
sumarN _ [] = []
sumarN n l | longitud l == 1 = (head(l)+n):[]
           | otherwise = (head(l)+n):(sumarN n (tail(l)))

sumarElPrimero :: [Int] -> [Int]
sumarElPrimero [] = []
sumarElPrimero l = (sumarN (head l) (l))

sumarElUltimo :: [Int] -> [Int]
sumarElUltimo [] = []
sumarElUltimo l = reverso (sumarElPrimero (reverso (l)))

pares :: [Int] -> [Int]
pares [] = []
pares l | longitud l == 1 && (mod (head(l)) 2)==0 = (head(l)):[]
        | (mod (head(l)) 2)==0 = (head(l)):(pares (tail(l)))
        | (mod (head(l)) 2)==1 = pares (tail(l))

multiplosDeN :: Int -> [Int] -> [Int]
multiplosDeN _ [] = []
multiplosDeN n l | longitud l == 1 && (mod (head(l)) n)==0 && (head(l))/=0 = (head(l)):[]
                 | longitud l == 1 && (mod (head(l)) n)==0 && (head(l))==0 = []
                 | (mod (head(l)) n)==0 && (head(l))/=0 = (head(l)):(multiplosDeN n (tail(l)))
                 | (mod (head(l)) n)/=0 && (head(l))/=0 = (multiplosDeN n (tail(l)))
                 | head(l)==0 = (multiplosDeN n (tail(l)))


quitar :: Int -> [Int] -> [Int]
quitar n l = quitar2 n l False

quitar2 :: Int -> [Int] -> Bool -> [Int]
quitar2 _ [] _ = []
quitar2 n l a | longitud l == 1 && a = head(l):[]
              | head(l)==n && a==False = quitar2 n (tail(l)) True
              | otherwise = head(l):(quitar2 n (tail(l)) a)

hayRepetidos :: [Int] -> Bool
hayRepetidos [] = False
hayRepetidos l = pertenece (head(l)) (tail(l))

eliminarRepetidos :: [Int] -> [Int]
eliminarRepetidos [] = []
eliminarRepetidos l | longitud (l) == 1 = (head (l)):[]
eliminarRepetidos l | pertenece (head(l)) (tail(l)) = eliminarRepetidos (tail(l))
                    | otherwise = head(l):(eliminarRepetidos (tail(l)))

maximo2 :: Int -> Int -> Int
maximo2 a b | a>b = a
            | otherwise = b

maximo :: [Int] -> Int
maximo [] = 0
maximo l | longitud l == 1 = head(l)
         | longitud l == 2 = maximo2 (head(l)) (head(tail(l)))
         | otherwise = maximo2 (head(l)) (maximo(tail(l)))

ordenar :: [Int] -> [Int]
ordenar [] = []
ordenar l = ordenar2 l []

ordenar2 :: [Int] -> [Int] -> [Int]
ordenar2 [] b = b
ordenar2 l b | longitud l == 1 = head(l):b
             | otherwise = ordenar2 (quitar (maximo (l)) l) ((maximo (l)):b)

reverso :: [a] -> [a]
reverso [] = []
reverso l = reverso2 l []

reverso2 :: [a] -> [a] -> [a]
reverso2 [] b = b
reverso2 l b | longitud l == 1 = head(l):b
             | otherwise = reverso2 (tail(l)) (head(l):b)

esMultiploDe :: Int -> Int -> Bool
esMultiploDe x1 x2 | mod x1 x2 == 0 = True
                   | otherwise = False

esPrimo :: Integer -> Bool                  --La funcion para ver si es primo de la clase 5
esPrimo 1 = False
esPrimo n |n == menorDiv n = True
          |otherwise = False

menorDiv :: Integer -> Integer
menorDiv n = menorDiv2 n 2

menorDiv2 :: Integer -> Integer -> Integer
menorDiv2 n r |mod n r == 0 = r
              |otherwise = menorDiv2 n (r+1)



es2Pseudoprimo :: Integer -> Bool
es2Pseudoprimo n = esAPseudoprimo n 2


es3Pseudoprimo :: Integer -> Bool
es3Pseudoprimo n = esAPseudoprimo n 3

esAPseudoprimo :: Integer -> Integer -> Bool            -- Primero se fija si el n es primo, si lo es no puede ser Apseudoprimo por definicion     
esAPseudoprimo n m |esPrimo n == True = False
                   |mod (m^(n-1)-1) n == 0 = True
                   |otherwise = False


sonCoprimos :: Integer -> Integer -> Bool 
sonCoprimos n m = sonCoprimos2 n m m                      -- empieza un contador en el numero m
                                                          -- cuando llega a 1 significa que no hubo ningun divisor en comun entonces cumple
sonCoprimos2 :: Integer -> Integer -> Integer -> Bool
sonCoprimos2 n m r |r == 1 = True
                   |mod n r == 0 && mod m r == 0 = False  -- chequea si el r es divisor de ambos y si lo es devuelve falso
                   |otherwise = sonCoprimos2 n m (r-1)    -- si no divide a ambos baja el r



cantidad3Pseudoprimos :: Integer -> Integer
cantidad3Pseudoprimos m = cantidad3Pseudoprimos2 m m

cantidad3Pseudoprimos2 :: Integer -> Integer -> Integer         
cantidad3Pseudoprimos2 m n |n == 1 = 0                                            -- uso el 1 como el caso base
                           |es3Pseudoprimo n = 1 + cantidad3Pseudoprimos2 m (n-1) -- si el n es 3pseudoprimo agrega 1 y baja el n
                           |otherwise = cantidad3Pseudoprimos2 m (n-1)            -- sino lo deja igual y baja el n


esCarmichael :: Integer -> Bool                 --ANDA MAL, A VECES DICE TRUE CUANDO NO DEBERIA.
esCarmichael n = esCarmichael2 n (n-1)

esCarmichael2 :: Integer -> Integer -> Bool     --
esCarmichael2 1 m = False                       -- Tengo que definir cuando el n es 1 porque "1" cuenta como no primo y es divisor de todo
esCarmichael2 n m |esPrimo n = False            -- Como un numero de Carmichael es un numero natural compuesto si es primo no puede cumplir
                  |m == 1 = True                -- La funcion va probando con todos los a menores o iguales a n-1, si llega a 1 quiere decir 
                  |esAPseudoprimo n m == True = esAPseudoprimo n (m-1)  --que es Apseudoprimo de todos los numeros entre 1 y n-1, entonces cumple
                  |otherwise = False            -- si no fuera Apseudoprimo de algun numero devuelve falso

sonIguales :: Integer -> Integer -> Bool
sonIguales n m |n == m = True
               |otherwise = False

es2y3Pseudoprimo :: Integer -> Bool                                              --Se fija si es 2pseudoprimo y 3pseudoprimo a la vez
es2y3Pseudoprimo n |es2Pseudoprimo n == True && es3Pseudoprimo n == True = True
                   |otherwise = False

kesimo2y3Pseudoprimo :: Integer -> Integer                               
kesimo2y3Pseudoprimo n = kesimo2y3Pseudoprimo2 n 1105 1            -- el n es el numero que se le da al programa, como el primer numero que cumple
                                                                   -- ambas condiciones simultaneamente es el 1005 supuse que si el m empezaba
kesimo2y3Pseudoprimo2 :: Integer -> Integer -> Integer -> Integer  -- en el 1005 iba a ser ligeramente mas rapido
kesimo2y3Pseudoprimo2 n m r |es2y3Pseudoprimo m && n==r = m        -- verifica que cumpla que es 2y3pp y a la vez que es el k-esimo
                |es2y3Pseudoprimo m = kesimo2y3Pseudoprimo2 n (m+1) (r+1) --si no era el kesimo se fija si cumple que es 2y3pp, si es sube el r
                |otherwise = kesimo2y3Pseudoprimo2 n (m+1) r       -- si no cumple ninguna aumenta el m y vuelve a chequear



kesimo2y3Pseudoprimo' :: Int -> Integer                           -- ¿Hay necesidad de que la funcion sea de tipo Integer -> Integer?    
kesimo2y3Pseudoprimo' n = kesimo2y3Pseudoprimo2' n 1105 1          -- Justo para esta funcion si alguien pide que el k sea un numero tan grande
                                                                  -- que no pueda estar en los enteros, no creo que nadie tenga la paciencia
kesimo2y3Pseudoprimo2' :: Int -> Integer -> Int -> Integer        -- para esperar hasta que termine     
kesimo2y3Pseudoprimo2' n m r |es2y3Pseudoprimo m && n==r = m      
                             |es2y3Pseudoprimo m = kesimo2y3Pseudoprimo2' n (m+1) (r+1)
                             |otherwise = kesimo2y3Pseudoprimo2' n (m+1) r

prodInt :: (Float, Float) -> (Float, Float) -> Float
prodInt (x1,y1) (x2,y2) = x1*x2 + y1*y2

todoMenor :: (Float, Float) -> (Float, Float) -> Bool
todoMenor (x1,y1) (x2,y2) | x1<x2 && y1<y2 = True
                          | otherwise = False

normaVectorial :: (Float, Float) -> Float
normaVectorial (x1,y1) = sqrt(x1*x1+y1*y1)

distanciaPuntos :: (Float, Float) -> (Float, Float) -> Float
distanciaPuntos (x1,y1) (x2,y2) = normaVectorial(x2-x1,y2-y1)

sumaTerna :: (Int, Int, Int) -> Int
sumaTerna (x1,x2,x3) = x1+x2+x3

posicPrimerPar :: (Int, Int, Int) -> Int
posicPrimerPar (x1,x2,x3) | mod x1 2 == 0 = 1
                          | mod x2 2 == 0 = 2
                          | mod x3 2 == 0 = 3
                          | otherwise = 4

crearPar :: a -> b -> (a,b)
crearPar a b = (a,b)

invertir :: (a,b) -> (b, a)
invertir (a,b) = (b,a) 

concatenar :: [Int] -> [Int] -> [Int]
concatenar [] b = b
concatenar a b = reverso2 (reverso2 a []) b 

zipi :: [a] -> [b] -> [(a,b)]
zipi [] _ = []
zipi _ [] = []
zipi c d = (head(c),head(d)): zipi (tail(c)) (tail(d))

