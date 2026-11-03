import Auxiliar

{-
    Funcion : multiEnt
    Descripcion: Toma a los dos enteros y regresa el producto de ellos, si uno es cero el resultado es cero, se aplica la ley de los signos y ya
    Uso: multiEnt (Neg (Succ (Succ (Succ (Succ Zero))))) (Succ (Succ Zero)) = Neg (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero))))))))
-}

multiEnt :: Entero -> Entero -> Entero
multiEnt Zero m = Zero
multiEnt n Zero = Zero
multiEnt (Neg n) (Neg m) = multiEnt n m
multiEnt (Neg n) m = Neg (multiEnt n m)
multiEnt n (Neg m) = Neg (multiEnt n m)
multiEnt (Succ n) m = sumaPositivos m (multiEnt n m)