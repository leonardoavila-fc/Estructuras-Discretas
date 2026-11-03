
{-
    Tipo : Matrioska
    Descripcion: es el dato de las matioskas, siendo Mati la mas chica que no tiene nada adentro, Cont m es la muñeca que tiene a la matrioska m.
    Uso: Mati , Cont (Cont Mati)
-}

data Matrioska = Mati | Cont Matrioska deriving (Eq, Show)

{-
    Funcion : mayorIgual
    Descripcion: Recibe dos matrioskas y regresa True si la primera es mayor o igual a la segunda. Se van abriendo a la par: si a la segunda se le acaban las muñecas primero o al mismo tiempo, la primera es mayor o igual. En cambio, si la primera llega al final (Mati) y a la segunda todavía le quedan piezas, resulta ser menor.
    Uso: mayorIgual Mati Mati = True , mayorIgual (Cont (Cont Mati)) (Cont (Cont (Cont Mati))) = False
-}

mayorIgual :: Matrioska -> Matrioska -> Bool
mayorIgual m Mati = True
mayorIgual Mati (Cont m) = False
mayorIgual (Cont n) (Cont m) = mayorIgual n m

{-
    Funcion : aplana
    Descripcion:Toma una matrioska y devuelve una lista con las matrioskas que la forman, de la mas grande a la mas chica. Se pone la matrioskas completa al inicio para despues aplanar lo que tiene dentro hasta llegar a Mati
    Uso: aplana Mati = [Mati] , aplana (Cont (Cont Mati)) = [Cont (Cont Mati), Cont Mati, Mati]
-}

aplana :: Matrioska -> [Matrioska]
aplana Mati = [Mati]
aplana (Cont m) = Cont m : aplana m