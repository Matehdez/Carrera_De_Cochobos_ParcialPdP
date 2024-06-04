module Library where
import PdePreludat
import GHC.Generics ((:.:)(unComp1))

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

tiempoTotal :: Pista -> Cochobo -> Number
tiempoTotal unaPista unCochobo = sum (tiemposDeCochobo unCochobo unaPista)

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
compararTiempos unaPista jineteA jineteB = tiempoTotal unaPista (cochoboDeJinete jineteA) < tiempoTotal unaPista (cochoboDeJinete jineteB)


