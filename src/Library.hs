module Library where
import PdePreludat
import GHC.Generics ((:.:)(unComp1))
import Control.Monad.ST.Unsafe (unsafeDupableInterleaveST)


---Pistas-------------------------------
type Pista  = [Tramo]
type Tramo = (Number,Cochobo->Number)


bosqueTenebroso =
    [(100, f1), (50, f2), (120, f2), (200, f1), (80, f3)]
pantanoDelDestino =
    [(40, f2), (90, \(f,p,v)-> f + p + v), (120, fuerza), (20, fuerza)]

---FuncionesChocobos-----------------------------
f1 chocobo = velocidad chocobo * 2
f2 chocobo = velocidad chocobo + fuerza chocobo
f3 chocobo = velocidad chocobo / peso chocobo

---Chocobos-------------------------------------
type Cochobo = (Number, Number, Number)

amarillo = (5, 3, 3)
negro = (4, 4, 4)
blanco = (2, 3, 6)
rojo = (3, 4, 4)

---FuncionInicializacion-------------------------
fuerza (f,_,_) = f
peso (_,p,_) = p
velocidad (_,_,v) = v

---jinetes de los chocobos------------------------
type Jinete = (String, Cochobo)
apocalipsis =
    [("Leo", amarillo), ("Gise", blanco), ("Mati", negro), ("Alf",rojo)]

{--Disponemos de esta función a modo de ayuda que, 
a partir de una lista y un criterio de ordenamiento, 
nos devuelve la versión equivalente a esa lista pero con los elementos ordenados por el criterio dado.--}
quickSort _ [] = []
quickSort criterio (x:xs) =
    (quickSort criterio . filter (not . criterio x)) xs
    ++ [x] ++
    (quickSort criterio . filter (criterio x)) xs

{--Ejemplo de uso:
> quickSort (>) [3,8,7,20,2,1]
[20,8,7,3,2,1]--}

---Pto1------------------------------------------------------------------
{-|
Definir dos funciones `mayorSegun` y `menorSegun` que, 
dados una función y dos valores, nos dice si el resultado de
evaluar la función para el primer valor es mayor / menor que el resultado de evaluar la función para 
el segundo.
-}

---Ejemplo de uso
{-- > mayorSegun length bosqueTenebroso pantanoDelDestino
True (tiene 5 tramos el bosque y 4 tramos el pantano) --}

mayorSegun :: Ord b => (a -> b) -> a -> a -> Bool
mayorSegun f x y = f x > f y

menorSegun :: Ord b => (a->b) -> a -> a -> Bool
menorSegun f x y = f x < f y

---Pto2------------------------------------------------------------------
---a
-- Saber el tiempo que tarda un chocobo en recorrer un tramo. 
--El mismo está dado por la distancia del tramo  dividido por la velocidad corregida para el chocobo.

---Ej
-- > tiempo amarillo (head bosqueTenebroso)
-- 16

tiempo :: Cochobo -> Tramo -> Number
tiempo unCochobo unTramo= fst unTramo `div` correccionVelocidad unTramo unCochobo

correccionVelocidad :: Tramo -> (Cochobo->Number)
correccionVelocidad (_,f) = f

---b
---Determinar el tiempo total de un chocobo en una carrera.

---Ej
--  > tiempoTotal bosqueTenebroso amarillo
--150
tiemposDeCochobo :: Cochobo -> Pista -> [Number]
tiemposDeCochobo unCochobo = map (tiempo unCochobo)

tiempoTotal :: Cochobo -> Pista -> Number
tiempoTotal  unCochobo unaPista = sum (tiemposDeCochobo unCochobo unaPista)

---Pto3------------------------------------------------------------------
--Obtener el podio de una carrera, 
--representado por una lista ordenada de los 3 primeros puestos de la misma, 
--en base a una lista de jinetes y una pista. El puesto está dado por el tiempo total, 
--de menor a mayor y se espera obtener una lista de jinetes.

---Ej
-- > podio bosqueTenebroso apocalipsis
-- [("Gise",(2,3,6)),("Mati",(4,4,4)),("Alf",(3,3,4))] (ver también ejemplo del punto 6)

podio :: Pista -> [Jinete] -> [Jinete]
podio unaPista= quickSort (compararTiempos unaPista)

cochoboDeJinete :: Jinete -> Cochobo
cochoboDeJinete (_, unCochobo) = unCochobo

compararTiempos :: Pista-> Jinete -> Jinete -> Bool
compararTiempos unaPista jineteA jineteB = tiempoTotal (cochoboDeJinete jineteA) unaPista  < tiempoTotal (cochoboDeJinete jineteB) unaPista 

---Pto4------------------------------------------------------------------
---a
--Realizar una función que dado un tramo y una lista de jinetes, 
--retorna el nombre de aquel que lo recorrió en el menor tiempo.

---Ej
-- > elMejorDelTramo (head bosqueTenebroso) apocalipsis
--"Gise" (Gise tarda 8, mientras que Leo tarda 16 y Mati y Alf tardan 12)

elMejorDelTramo :: Tramo -> [Jinete] -> String
elMejorDelTramo unTramo  = nombreJinete.head.quickSort (compararTramos unTramo)

compararTramos ::  Tramo-> Jinete -> Jinete -> Bool
compararTramos unTramo jineteA jineteB = tiempo (cochoboDeJinete jineteA) unTramo < tiempo (cochoboDeJinete jineteB) unTramo

nombreJinete :: Jinete -> String
nombreJinete (nombre, _ ) = nombre

---ORDEN SUPERIOR DE FUNCION COMPARAR
comparar :: Ord a => (a -> b -> Number) -> a -> a -> b -> Bool
comparar criterio a c b = criterio a b > criterio c b


---b
--Dada una pista y una lista de jinetes, 
--saber el nombre del jinete que ganó más tramos (que no quiere decirque haya ganado la carrera).

--Ej:
-- > elMasWinner pantanoDelDestino apocalipsis
-- "Leo" (gana 2 tramos, el resto gana 1 o ninguno)


elMasWinner :: Pista -> [Jinete] -> String
elMasWinner unaPista  =  nombreJinete.head.quickSort (ganoMasTramos unaPista) 

ganoMasTramos :: Pista -> Jinete -> Jinete -> Bool
ganoMasTramos unaPista jineteA jineteB = not (null (tramosGanados unaPista [jineteA, jineteB] jineteA))

tramosGanados :: Pista -> [Jinete] -> Jinete -> Pista
tramosGanados unaPista jinetes jinete = filter (esElMejorDelTramo jinete jinetes) unaPista

esElMejorDelTramo :: Jinete -> [Jinete] -> Tramo -> Bool
esElMejorDelTramo unJinete jinetes unTramo = nombreJinete unJinete == elMejorDelTramo unTramo jinetes

---Pto5------------------------------------------------------------------
--Saber los nombres de los jinetes que pueden hacer un tramo dado en un tiempo indicado máximo..

--Ej
-- > quienesPueden (head bosqueTenebroso) 12 apocalipsis
--["Gise","Mati","Alf"] (ver 4.a)

quienesPueden :: Tramo -> Number -> [Jinete] -> [String]
quienesPueden unTramo n jinetes = map nombreJinete (jinetesQuePueden unTramo n jinetes)

jinetesQuePueden :: Tramo -> Number -> [Jinete] -> [Jinete]
jinetesQuePueden unTramo n  = filter (tiempoMax unTramo n) 

tiempoMax :: Tramo -> Number -> Jinete -> Bool
tiempoMax unTramo n unJinete = tiempo (cochoboDeJinete unJinete) unTramo <= n

---Pto6------------------------------------------------------------------
--Obtener las estadísticas de una carrera, dada la pista y la lista de jinetes. 
--Estas estadísticas deben estar representadas por una lista de tuplas, 
--cada tupla siendo de la forma: (nombre, tramosGanados, tiempoTotal)

---Ej
-- > estadisticas bosqueTenebroso apocalipsis
--[("Leo",0,150),("Gise",3,85),("Mati",2,138),("Alf",0,141)]

type Estadisticas = (String, Number, Number)

estadisticas :: Pista -> [Jinete] -> [Estadisticas]
estadisticas unaPista jinetes = map (jineteEnCarrera unaPista jinetes) jinetes

jineteEnCarrera :: Pista-> [Jinete] -> Jinete -> Estadisticas
jineteEnCarrera unaPista jinetes unJinete  = (nombreJinete unJinete, tramosGanadosEnNumero unaPista jinetes unJinete, tiempoTotal (cochoboDeJinete unJinete) unaPista )

tramosGanadosEnNumero :: Pista -> [Jinete] -> Jinete -> Number
tramosGanadosEnNumero unaPista jinetes jinete = length (tramosGanados unaPista jinetes jinete) 

---Pto7------------------------------------------------------------------
-- Saber si una carrera fue pareja. 
--Esto es así si cada chocobo tuvo un tiempo total de hasta 10% menor que el que llegó a continuación.

---Ej:
-- > fuePareja bosqueTenebroso apocalipsis
-- False (entre Gise y Mati, 1a y 2o respectivamente, hay más de 10% de diferencia)

fuePareja :: Pista -> [Jinete] -> Bool
fuePareja unaPista jinetes = foldl (\booleano jinete -> booleano && tiempoEsMenor unaPista jinete (head (tail jinetes))) True (podio unaPista jinetes)

porcentajeParejo:: Number
porcentajeParejo = 0.1

tiempoEsMenor :: Pista -> Jinete -> Jinete -> Bool
tiempoEsMenor unaPista jineteA jineteB = tiempoTotal (cochoboDeJinete jineteA) unaPista  < porcentajeParejo * tiempoTotal (cochoboDeJinete jineteB) unaPista 

---Pto8------------------------------------------------------------------
--Definir un chocobo plateado que tenga las mejores características de los otros 
--(mayor fuerza, menor peso, mayor velocidad), 
--teniendo en cuenta que no sea necesario cambiar su definición si se altera un valor de los anteriores.

---EJ:
-- > plateado
-- (5,3,6)

---PINCHO HASTA ACA LLEGUE XDXD





