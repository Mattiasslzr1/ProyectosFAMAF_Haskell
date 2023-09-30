-- 1)a) Verificar si es igual a 0

esCero :: Int -> Bool
esCero x | (x==0) = True
         | otherwise = False

-- 1)b) Verificar si un entero es esctrictamente mayor a 0

esPositivo :: Int -> Bool
esPositivo x | x > 0 = True
             | x < 0 = False


-- 1)c) Verifica si un caracter es vocal en minuscula

esVocal :: Char -> Bool
esVocal x | (x == 'a'  || x == 'e' || x == 'i'  || x == 'o' || x == 'u') = True
          | otherwise = False 


-- 1)d) Verificar que devuelva el valor absoluto de un entero ingresado

valorAbsoluto :: Int -> Int
valorAbsoluto x | (x < 0)  = -x 
                | otherwise  = x

-- 2)a) Verifica que todo los elementos de una lista sean True

paraTodo :: [Bool] -> Bool
paraTodo [] = True
paraTodo (x:xs) = (x == True) && paraTodo xs  


-- 2)b) Calcula la suma de todos los elementos de una lista de enteros.
sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs


-- 2)c) Calcula el producto de todos los elementos de la lista de enteros.
productoria :: [Int] -> Int
productoria [] = 1 
productoria (x:xs) = x * productoria xs

-- 2)d) toma un numero n y calcula n!.
factorial :: Int -> Int
factorial x | (x == 0) = 1 
            | otherwise = x * factorial (x -1)


-- 2)e) toma una lista de numeros no vacia y calcula el valor promedio    
promedio :: [Int] -> Int
promedio [] = 0
promedio (x:xs) = div (sumatoria xs) (length xs)


-- 3) Programa la funcion pertenece :: Int -> [Int] -> Bool, que verifica si un nu ́mero se encuentra en una lista.
pertenece :: Int -> [Int] -> Bool 
pertenece n [] = False  
pertenece n (x:xs) | (n == x) = True 
                   | (n /= x) = False


-- 4)a) dada una lista xs de tipo [a] y un predicado t :: a -> Bool, determina si todos los elementos de xs satisfacen el predicado t.               
paratodo' :: [a] -> ( a -> Bool) -> Bool
paratodo' [] t = True
paratodo' (a:as) t = (t a) && (paratodo' as t)


-- 4)b) dada una lista xs de tipo [a] y un predicado t :: a -> Bool, determina si algu ́n elemento de xs satisface el predicado t.
existe' :: [a] -> (a -> Bool) -> Bool 
existe' [] t = False 
existe' (a:as) t = (t a) || existe' as t


-- 4)c) dada una lista xs de tipo [a] y una funci ́on t :: a -> Int (toma elementos de tipo a y devuelve enteros), calcula la suma de los valores que resultan de la aplicaci ́on de t a los elementos de xs.
sumatoria' :: [a] -> (a -> Int) -> Int
sumatoria' [] t = 0
sumatoria' (a:as) t = t a + sumatoria' as t


-- 4)d) dada una lista de xs de tipo [a] y una funci ́on t :: a -> Int, calcula el producto de los valores que resultan de la aplicaci ́on de t a los elementos de xs.
productoria' :: [a] -> (a -> Int) -> Int
productoria' [] _ = 1
productoria' (a:as) t = t a * productoria' as t

--5)  Escribi un nuevo paratodo, pero esta vez usando la funcion para todo' sun recursion ni analisis de caso

paratodo :: [a] -> (a -> Bool) -> Bool
paratodo xs t = paratodo' xs t


-- 6)a)  todosPares :: [Int] -> Bool verifica que todos los numeros de una lista sean pares.
esPar :: Int -> Bool
esPar x = x `mod` 2 ==0 

todosPares :: [Int] -> Bool
todosPares x = (paratodo' x) esPar


-- 6)b) hayMultiplo :: Int -> [Int] -> Bool verifica si existe algun numero dentro del segundo parametro que sea multiplo del primer parametro.
hayMultiplo :: Int -> [Int] -> Bool
hayMultiplo n xs = existe' xs (\x -> x `mod` n == 0)  

-- 6)c) sumaCuadrados :: Int -> Int, dado un nu ́mero no negativo n, calcula la suma de losprimerosncuadrados,esdecir􏰀􏰃i :0≤i<n: i2􏰁.
sumaCuadrados :: Int -> Int
sumaCuadrados x = sumatoria' [0..x] (^2)

-- 6)d) Programa la funcion existeDivisor :: int -> [ Int] -> Bool, que dado un entero n y una lista ls, devuelve true si y solo si n es primo
existeDivisor :: Int -> [Int] -> Bool
existeDivisor _ [] = False
existeDivisor n (x:xs)  | n `mod` x == 0 = True 
                        | otherwise = existeDivisor n xs

-- 6) e) Programa numeros primos
esPrimo :: Int -> Bool
esPrimo n
  | n <= 1 = False
  | n == 2 = True
  | n `mod` 2 == 0 = False
  | otherwise = esPrimoAux 3
  where
    esPrimoAux :: Int -> Bool
    esPrimoAux k
      | k * k > n = True
      | n `mod` k == 0 = False
      | otherwise = esPrimoAux (k + 2)

-- 6)f) Factorial utilizando recursion 

factorial' :: Int -> Int
factorial' x = productoria' [1..x] (+0)

--6)g) 

multiplicarprimos :: [Int] -> Int
multiplicarprimos xs = productoria' (filter esPrimo xs)
  where
    productoria' :: [Int] -> Int
    productoria' [] = 1
    productoria' (x:xs) = x * productoria' xs


--6) h) Calcular fibonacci y utiliza un auxiliar.

esFib :: Int -> Bool
esFib n = esFibAux n 0 1
  where
    esFibAux :: Int -> Int -> Int -> Bool
    esFibAux n a b
      | n == a = True
      | n < a = False
      | otherwise = esFibAux n b (a + b)


-- 6) i) Definir la funcion todosFib.

todosFib :: [Int] -> Bool
todosFib [] = True
todosFib (x:xs) = esFib x && todosFib xs

-- 7)a ¿Que hacen estas funciones?
-- Lo que realiza la funcion map es transforma una lista de valores en otra lista de valores aplicando una función a cada uno de ellos.
-- Lo que hace la funcion filter es filtra los elementos de una lista según una condición especificada por una función.

-- 7)b) ¿A que equivale la expresion map succ [1, -4, 6, 2, -8], donde succ n = n+1?
-- Lo que realiza la funcion map succ es mapear los elementos de la lista y dar los el siguiente numero de el elemento en la lista 
--por Ej:  map succ [1, -4 , 6 ,2 ,-8], esa lista le sumara 1 mas a cada eleemnto y nos devolvera
--[2, -3 , 7 , 3 ,-7]

-- 7)c) ¿Y la expresion filter esPositivo [1, -4, 6, 2, -8]?
-- La funcion filtrara los numeros negativos de la lista devolviendonos asi solo los positivos
-- En este caso [1, 6 , 2]

-- 8)Programa una funcion que dada una lista de numeros xs, devuelve la lista que resulta de duplicar cada valor de xs.
-- a) Recursivamente
duplicarLista :: [Int] -> [Int]
duplicarLista [] = [] 
duplicarLista (x:xs) = 2 * x : duplicarLista xs

-- b)Definila utilizando la funcion map.

duplicarListaa :: [Int] -> [Int]
duplicarListaa xs = map (*2) xs


-- 9) Programa una funcion que dada una lista de nu ́meros xs, calcula una lista que tiene como elementos aquellos numeros de xs que son primos.
-- a) Definila usando recursion.
numerosPrimos :: [Int] -> [Int]
numerosPrimos [] = []
numerosPrimos (x:xs) | esPrimo x = x : numerosPrimos xs
                     | otherwise = numerosPrimos xs


-- b) Definala usando filter                    
numerosPrimos' :: [Int] -> [Int]
numerosPrimos' xs = filter esPrimo xs


-- c) Una forma de mejorarla puede ser 
multiplicaPrimos' :: [Int] -> Int
multiplicaPrimos' xs = foldr (*) 1 (filter esPrimo xs)

-- 10)  La funcio ́n primIgualesA toma un valor y una lista, y calcula el tramo inicial m ́as largo de la lista cuyos elementos son iguales a ese valor. Por ejemplo:
-- primIgualesA 3 [3,3,4,1] = [3,3]
-- primIgualesA 3 [4,3,3,4,1] = []
-- primIgualesA 3 [] = []
-- primIgualesA ’a’ "aaadaa" = "aaa"


-- 10)a) Programa primIgualesA por recursion
primIgualesA :: Eq a => a -> [a] -> [a]
primIgualesA _ [] = []
primIgualesA x (y:ys) | x == y = x : primIgualesA x ys
                      | otherwise = []

-- 10)b) Programa nuevamente la funci ́on utilizando takeWhile.                      
primIgual :: Eq a => a -> [a] -> [a]
primIgual x = takeWhile (== x)

-- 11) La funcion primIguales toma una lista y devuelve el mayor tramo inicial de la lista cuyos elementos son todos iguales entre sı. Por ejemplo:
-- primIguales [3,3,4,1] = [3,3]
-- primIguales [4,3,3,4,1] = [4]
-- primIguales [] = []
-- primIguales "aaadaa" = "aaa"


-- 11)a) Programa primIguales por recursion.
primIguales :: Eq a => [a] -> [a]
primIguales [] = []
primIguales [x] = [x]
primIguales (x:y:xs) | x == y = x : primIguales (y:xs) 
                     | otherwise = [x]


--11)b) Usa ́ cualquier versio n de primIgualesA para programar primIguales. Esta  permitido dividir en casos, pero no usar recursi ́on.
priMIguales :: Eq a => [a] -> [a]
priMIguales xs = primIgualesA (head xs) xs


-- 12) (*) Cuantificador general 

cuantGen :: (b -> b -> b) -> b -> [a] -> (a -> b) -> b
cuantGen op z [] t = z
cuantGen op z (x:xs) t = t x `op` cuantGen op z xs t

-- existen xs t = any t xs
existe'' :: [a] -> (a -> Bool) -> Bool 
existe'' xs t = cuantGen (||) False xs t

-- paratodo xs t = all t xs
paratodo'' :: [a] -> (a -> Bool) -> Bool
paratodo'' xs t = cuantGen (&&) True xs t

-- sumatoria xs t = sum (map t xs)
sumatoria'' :: [a] -> (a -> Int) -> Int
sumatoria'' xs t = cuantGen (+) 0 xs t

-- productoria xs t = product (map t xs)
productoria'' :: [a] -> (a -> Int) -> Int
productoria'' xs t = cuantGen (*) 1 xs t


-- 13) (*) Para cada uno de los siguientes patrones, decid ́ı si esta ́n bien tipados, y en tal caso da los tipos de cada subexpresi ́on. En caso de estar bien tipado, ¿el patr ́on cubre todos los casos de definici ́on?
-- a) f :: (a, b) -> ... f (x , y) = ...
-- b) f :: [(a, b)] -> ... f (a , b) = ...
-- c) f :: [(a, b)] -> ... f (x:xs) = ...
-- d) f :: [(a, b)] -> ... f ((x, y) : ((a, b) : xs)) = ...
-- e) f :: [(Int, a)] -> ... f [(0, a)] = ...

-- a) Bien tipado aunque no se especifica si cubre todos los casos de definición, pero se entiende que (a = x) y (b= y) 
-- b) Esta mal tipado (a,b ) no puede coincidir con una lista de pares
-- c) Bien tipado pero no se especifica si cubre todos los casos de definición.
-- d) Bien tipado pero no se especifica si cubre todos los casos de definición.
-- e) Esta bien tipado y cubre todo los casos!!

-- 14) Para las siguientes declaraciones de funciones, da al menos una definicion cuando sea posible. ¿Podes dar alguna otra definicion alternativa a la que diste en cada caso?

--a)f :: (a, b) -> b
--  f :: (a, b) -> b
--  f (_, y) = y  
 

--otra puede ser 


--  f :: (a, b) -> b
--  f (x, y) = snd (x, y)


-- b)f :: (a, b) -> c
-- No se puede debido a que no conocemos C


-- c)f :: (a -> b) -> a -> b 
--   f :: (a -> b) -> a -> b
--   f g x = g x


-- d)f :: (a -> b) -> [a] -> [b]
--   f :: (a -> b) -> [a] -> [b]
--   f g = map g


-- e)f :: (a -> b) -> (b -> c) -> a -> c
--   f :: (a -> b) -> (b -> c) -> a -> c
--   f g h x = h (g x)
  
--Otra puede ser 
-- f :: (a -> b) -> (b -> c) -> a -> c
-- f g h = (h . g)










-- Matias Leonel Salazar
-- Dni : 42.898.990
-- Matias.salazar@mi.unc.edu.ar