
import Auxiliar

{-
    Funcion : a_natural
    Descripcion: Recibe un numero entero y lo convierte en Natural. 
    Uso: a_natural 0 = Cero , a_natural 2 = S (S Cero)
-}

a_natural :: Int -> Natural
a_natural 0 = Cero
a_natural n = S (a_natural (n - 1))

{-
    Funcion : a_entero
    Descripcion:Recibe un natural (suc) y lo convierte a un entero como 1,2,3... 
    Uso: a_entero Cero = 0 , a_entero (S (S (S Cero))) = 3
-}

a_entero :: Natural -> Int
a_entero Cero = 0
a_entero (S n) = 1 + a_entero n

{-
    Funcion : potenciaNat
    Descripcion: Esta función recibe dos números naturales y calcula la potencia, elevando la base (el primero) al exponente indicado (el segundo). Si el exponente es Cero, el resultado es S Cero (equivalente a 1); en caso contrario, multiplica la base por el resultado de la potencia con un exponente menor, utilizando la función multiNat del módulo auxiliar.
    Uso: potenciaNat (S (S (S Cero))) (S (S Cero)) = S (S (S (S (S (S (S (S (S Cero))))))))
-}

potenciaNat :: Natural -> Natural -> Natural
potenciaNat b Cero = S Cero
potenciaNat b (S e) = multiNat b (potenciaNat b e)

{-
    Funcion : facNat
    Descripcion: Agarra al natural dado y lo pasa a su factorial.
    Uso: facNat (S (S (S Cero))) = S (S (S (S (S (S Cero)))))
-}

facNat :: Natural -> Natural
facNat Cero = S Cero
facNat (S n) = multiNat (S n) (facNat n)