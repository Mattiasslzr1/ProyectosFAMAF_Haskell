data Carrera = Matematica | Fisica | Computacion | Astronomia   deriving  (Show)

--b) Definir la siguiente funcion, usando pattern matching: titulo :: Carrera -> String que devuelve el nombre completo de la carrera en forma de string. 
-- Por ejemplo, para el constructor Matematica, debe devolver ”Licenciatura en Matematica”.


titulo :: Carrera -> String
titulo Matematica = "Licenciatura en Matematica"
titulo Fisica = "Licenciatura en Fisica"
titulo Computacion = "Licenciatura en Computacion"
titulo Astronomia = "Licenciatura en Astronomia"

--c) Definir el tipo NotaBasica con constructores Do, Re, Mi, Fa, Sol, La y Si

data NotaBasica = Do | Re | Mi | Fa | Sol | La | Si  deriving (Eq, Ord, Show)

--d) Completar la definici ́on del tipo NotaBasica para que las expresiones  *Main> Do <= Re *Main> Fa `min` Sol



cifradoAmericano :: NotaBasica -> Char
cifradoAmericano Do = 'C'
cifradoAmericano Re = 'D'
cifradoAmericano Mi = 'E'
cifradoAmericano Fa = 'F'
cifradoAmericano Sol = 'G'
cifradoAmericano La = 'A'
cifradoAmericano Si = 'B'

 
-- 3)a) Definir usando polimorfismo ad hoc la funcion minimoElemento 

minimoElemento :: Ord a => [a] -> a
minimoElemento [x] = x
minimoElemento (x:xs) = min x (minimoElemento xs)


--3)b) Definir la funcion minimoElemento de manera tal que el caso base de la recursion sea el de la lista vacıa. Para ello revisar la clase Bounded.

minimoElemento' :: (Ord a, Bounded a) => [a] -> a
minimoElemento' [] = maxBound
minimoElemento' (x:xs) = min x (minimoElemento' xs)


--3)c) Usar la funcion minimoElemento para determinar la nota mas grave de la melodıa: [Fa, La, Sol, Re, Fa]



melodia :: [NotaBasica]
melodia = [Fa, La, Sol, Re, Fa]

notaMasGrave :: NotaBasica
notaMasGrave = minimoElemento melodia


--a) Implementá el tipo Deportista y todos sus tipos accesorios (NumCamiseta, Altura, Zona, etc) tal como están definidos arriba.

--Sinonimo de tipo
type Altura = Int
type NumCamiseta = Int


--Tipo Algebraicos sin parametros 
data Zona = Arco | Defensa | Mediocampo | Delantero
data TipoReves = DosManos | UnaMano 
data Modalidad = Carretera | Pista | Monte | BMX
data PiernaHabil = Izquierda | Derecha


--Sinonimo
type ManoHabil = PiernaHabil



---Deportista es un tipo algebraico con contructores parametricos
data Deportista = Ajedrecista
                 | Ciclista Modalidad
                 | Velocista Altura 
                 | Tenista TipoReves ManoHabil Altura
                 | Futbolista Zona NumCamiseta PiernaHabil Altura



-- 4)b) ¿Cual es el tipo del constructor Ciclista?


-- -- El constructor es Deportista
-- Ciclista :: Modalidad -> Deportista


--4)c) Programá la función contar_velocistas :: [Deportista] -> Int que dada una lista de deportistas xs, devuelve la cantidad de velocistas que hay dentro de xs


deportistas :: [Deportista]
deportistas = [Ciclista Carretera, Velocista 180, Tenista DosManos Derecha 190, Velocista 175, Futbolista Defensa 5 Izquierda 170]



contar_velocistas :: [Deportista] -> Int 
contar_velocistas [] = 0 
contar_velocistas (x:xs) | esVelocista x  = 1 + contar_velocistas xs
                         | otherwise = contar_velocistas xs  
                         where esVelocista (Velocista _ ) = True
                               esVelocista _ = False


--4)d) Programá la función contar_futbolistas :: [Deportista] -> Zona -> Int que dada una lista de deportistas xs, y una zona z, devuelve la cantidad de futbolistas 
--incluidos en xs que juegan en la zona z



deportista' :: [Deportista]
deportista' = [Futbolista Defensa 5 Izquierda 170 , Futbolista Arco  1  Izquierda 170 , Futbolista Arco  2  Izquierda 180, Futbolista Defensa  3  Izquierda 193, Futbolista Defensa 5 Derecha 169, Futbolista Mediocampo 6 Derecha 170 , Futbolista Delantero 9 Izquierda 193, Futbolista Delantero 10 Derecha 176] 

contar_futbolistas :: [Deportista] -> Zona -> Int
contar_futbolistas [] _ = 0
contar_futbolistas (deportista' : deportistasRestantes) zonaBuscada =
    case deportista' of
        Futbolista z _ _ _ | z == zonaBuscada -> 1 + contar_futbolistas deportistasRestantes zonaBuscada
        _ -> contar_futbolistas deportistasRestantes zonaBuscada





-- 4)d) ¿La funcion anterior usa filter? Si no es ası, reprogramala para usarla.

--contar_futbolistas :: [Deportista] -> Zona -> Int
--contar_futbolistas deportistas zona = length (filter (\x -> esFutbolistaEnZona x zona) deportistas)

-- Función para verificar si un deportista es un futbolista en la zona especificada
--esFutbolistaEnZona :: Deportista -> Zona -> Bool
--esFutbolistaEnZona (Futbolista z _ _ _ _) zona = z == zona
--esFutbolistaEnZona _ _ = False


--5)a) Notas musicales 




data NotaMusical = Nota NotaBasica Alteracion deriving (Eq)

sonidoNatural :: NotaBasica -> Int 
sonidoNatural Do = 0 
sonidoNatural Re = 2 
sonidoNatural Mi = 4 
sonidoNatural Fa = 5 
sonidoNatural Sol = 7 
sonidoNatural La = 9
sonidoNatural Si = 11

--5)b)Definir el tipo enumerado Alteracion que consta de los constructores Bemol, Natural y Sostenido.

data Alteracion = Bemol | Sostenido | Natural deriving (Eq)

--5)c)Definir el tipo algebraico NotaMusical que debe tener un solo un constructor que llamaremos Nota el cual toma dos parámetros

miNota :: NotaMusical
miNota = Nota Do Sostenido  -- Representa la nota Do sostenido



--5)d) Definı la funcion sonidoCromatico :: NotaMusical -> Int

sonidoCromatico :: NotaMusical -> Int
sonidoCromatico (Nota nb Bemol) = sonidoNatural nb - 1
sonidoCromatico (Nota nb Sostenido) = sonidoNatural nb + 1
sonidoCromatico (Nota nb Natural) = sonidoNatural nb


--5)f)Incluı́ el tipo NotaMusical a la clase Eq de manera tal que dos notas que tengan el mismo valor de sonidoCromatico se consideren iguales.
instance Eq NotaMusical where
  (Nota nota1 alteracion1) == (Nota nota2 alteracion2) =
    (sonidoCromatico (Nota nota1 alteracion1)) == (sonidoCromatico (Nota nota2 alteracion2))

--f ) Incluı́ el tipo NotaMusical a la clase Ord definiendo el operador <=. Se debe definir
--que una nota es menor o igual a otra si y sólo si el valor de sonidoCromatico para la
--primera es menor o igual al valor de sonidoCromatico para la segunda

instance Ord NotaMusical where     
    a <= b = sonidoCromatico a <= sonidoCromatico b 


--6)a) Definir la funci ́on primerElemento que devuelve el primer elemento de una lista no vac ́ıa, o Nothing si la lista es vac ́ıa.



data Maybe a = Nothing | Just a deriving (Show,Eq)

    

primerElemento :: [a] -> Maybe a
primerElemento [] = Nothing
primerElemento (x:_) = Just x 


--7)1) atender :: Cola -> Maybe Cola, que elimina de la cola a la persona que esta en la primer posicion de una cola, por haber sido atendida. Si la cola esta vacıa, devuelve Nothing.

data Cola = VaciaC | Encolada Deportista Cola deriving (Show)

fila = encolar Velocista 190 (encolar (Velocista 189)(encolar (Velocista 170)(encolar (Velocista 190) VaciaC)))

atender :: Cola -> Maybe Cola
atender VaciaC = Nothing
atender (Encolada _ xs) = Just xs


--7)2) encolar::Deportista->Cola->Cola,que agrega una persona a una cola de personas, en la  ́ultima posici on.

encolar :: Deportista -> Cola -> Cola
encolar p VaciaC = Encolada p VaciaC
encolar p (Encolada x xs) = Encolada x (encolar p xs)

--7)3)--busca :: Cola -> Zona -> Maybe Deportista, que devuelve el/la primera
--futbolista dentro de la cola que juega en la zona que se corresponde con el segundo
--parámetro.

busca :: Cola -> Zona -> Maybe Deportista
busca VaciaC _ = Nothing
busca (Encolada p xs) zona = case p of
     Futbolista zonaJuego _ _ _ _ -> if zonaJuego == zona
                      then Just p
                      else busca xs zona
                    


--7)b) Cola se parece al tipo listas


--8)a) ¿Como se debe instanciar el tipo ListaAsoc para representar la informacion almacenadaen una guıa telefonica?

data ListaAsoc a b = Vacia | Nodo a b ( ListaAsoc a b) deriving (Show)

type Diccionario = ListaAsoc String String
type Padron = ListaAsoc Int String
type GuiaTelefonica = ListaAsoc String String

--8)b)

--1) Programar la funcion la_long:: ListaAsocab->Int   que devuelve la cantidad de datos en una lista.

la_long:: ListaAsoc a b -> Int  
la_long Vacia = 0
la_long (Nodo _ _ xs) = 1 + la_long xs


--2) la_concat :: ListaAsoc a b -> ListaAsoc a b -> ListaAsoc a b

la_concat Vacia ys = ys
la_concat (Nodo k v xs) ys = Nodo k v (la_concat xs ys)

lista1 :: ListaAsoc String String
lista1 = Nodo "clave1" "valor1" Vacia

lista2 :: ListaAsoc String String
lista2 = Nodo "clave3" "valor3" Vacia

resultado' :: ListaAsoc String String
resultado' = la_concat lista1 lista2


--3) la_agregar :: Eq a => ListaAsoc a b -> a -> b -> ListaAsoc a b, que agrega un nodo a la lista de asociaciones si la clave no está en la lista, o actualiza el valor si la clave ya se encontraba

la_agregar :: Eq a => ListaAsoc a b -> a -> b -> ListaAsoc a b
la_agregar Vacia k v = Nodo k v Vacia
la_agregar (Nodo k v xs) k' v'| k == k' = Nodo k v' xs
                               | otherwise = Nodo k v (agregar xs k' v')


--4) la_pares :: ListaAsoc a b -> [(a, b)] que transforma una lista de asociacion en una lista de pares clave-dato.

la_pares :: ListaAsoc a b -> [(a, b)]
la_pares Vacia = []
la_pares (Nodo k v xs) = (k, v) : lapares xs                   

ejemplo :: ListaAsoc String Int
ejemplo = Nodo "clave1" 1 (Nodo "clave2" 2 (Nodo "clave3" 3 Vacia))


-- print (lapares ej)


--5)la_busca :: Eq a => ListaAsoc a b -> a -> Maybe b que dada una lista y una clave devuelve el dato asociado, si es que existe. En caso contrario devuelve Nothing.

la_busca :: Eq a => ListaAsoc a b -> a -> Maybe b
la_busca Vacia _ = Nothing 
la_busca (Nodo k b xs) clave | k == clave = Just b
                             | otherwise = la_busca xs clave    


ejemplo' :: ListaAsoc String Int
ejemplo' = Nodo "clave1" 1 (Nodo "clave2" 2 (Nodo "clave3" 3 Vacia))                             

--ejemplo consola "  print (la_busca ejemplo "clave1") -- Just 1"
--                   print (la_busca ejemplo "clave4") -- Nothing


--6) la_borrar :: Eq a => a -> ListaAsoc a b -> ListaAsoc a b que dada una clave a elimina la entrada en la lista.

la_borrar :: Eq a => a -> ListaAsoc a b -> ListaAsoc a b
la_borrar _ Vacia = Vacia
la_borrar clave (Nodo k v xs)  | clave == k = xs
                               | otherwise = Nodo k v (la_borrar clave xs)

ejemplo'' :: ListaAsoc String Int
ejemplo'' = Nodo "clave1" 1 (Nodo "clave2" 2 (Nodo "clave3" 3 Vacia))



--Matias.salazar@mi.unc.edu.ar

-- Matias Leoenel salazar
-- 42.898.990