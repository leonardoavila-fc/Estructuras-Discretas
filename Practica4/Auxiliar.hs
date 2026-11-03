{-
 * Nombre:      Auxiliar.hs
 * Autor:       Avila R. Leonardo
 * Fecha:       2/11/2026
-}

module Auxiliar where

{-
    Tipo : Natural
    Aquí se define el tipo de dato para los números naturales (0 para el cero y S n para el sucesor, o n + 1). Lo colocamos en este módulo porque las funciones auxiliares dependen de él, evitando así el problema de las importaciones cruzadas que Haskell prohíbe.
    Uso: Cero , S (S Cero)
-}

data Natural = Cero | S Natural deriving (Eq, Show)

{-
    Tipo : Entero
    Aquí se definen los números enteros con Zero para el cero, Succ n para el sucesor y Neg n para el negativo. Al igual que con los naturales, va en este módulo para solucionar el problema de las dependencias cruzadas en Haskell.
    Uso: Zero , Succ (Succ Zero) , Neg (Succ Zero)
-}

data Entero = Zero | Succ Entero | Neg Entero deriving (Eq, Show)

{-
    Funcion : sumaNat
    Recibe dos naturales y calcula su suma paso a paso. Si el primero es Cero, simplemente regresa el segundo. Si no, le quita un nivel de sucesor (S) al primero y lo envuelve alrededor del resultado final.
    Uso: sumaNat (S Cero) (S (S Cero)) = S (S (S Cero))
-}

sumaNat :: Natural -> Natural -> Natural
sumaNat Cero m = m
sumaNat (S n) m = S (sumaNat n m)

{-
    Funcion : multiNat
    Recibe dos naturales y calcula su producto. La lógica se basa en sumar el segundo número tantas veces como marque el primero, de modo que cada S del primer número acumula una suma del segundo.
    Uso: multiNat (S (S Cero)) (S (S (S Cero))) = S (S (S (S (S (S Cero)))))
-}

multiNat :: Natural -> Natural -> Natural
multiNat Cero m = Cero
multiNat (S n) m = sumaNat m (multiNat n m)

{-
    Funcion : sumaPositivos
    Toma dos enteros (mayores o iguales a cero) y calcula su suma. Funciona exactamente igual que sumaNat, pero empleando Zero y Succ. Se limita a valores no negativos porque multiEnt lo necesita de esa forma para operar correctamente.
    Uso: sumaPositivos (Succ Zero) (Succ (Succ Zero)) = Succ (Succ (Succ Zero))
-}

sumaPositivos :: Entero -> Entero -> Entero
sumaPositivos Zero m = m
sumaPositivos (Succ n) m = Succ (sumaPositivos n m)