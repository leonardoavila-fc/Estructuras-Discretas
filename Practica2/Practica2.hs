{- Funcion: reconversion
   Descripcion: Pasa tu dinero de pesos a nuevos pesos
   Uso: reconversion 1000 = 1
-}

reconversion :: Float -> Float
reconversion dinero = dinero / 1000

{- Funcion: cashback
   Descripcion: Te da el cashback obtenido de una tarjeta de creditco con el 10% de puntps
   Uso:cashback 267 = 26.7
-}

cashback :: Float -> Float
cashback monto = monto / 10

{- Funcion: cashbackMonto
   Descripcion: Pasa tus puntos de cashback a pesos, dando tus puntos y su equivalenvia
   Uso: cashbackMonto 267 0.10 = 26.7
-}

cashback_monto :: Float -> Float -> Float
cashback_monto puntos valorPuntos = puntos * valorPuntos

{- Funcion: minutosHoras
   Descripcion: Recibe los minutos y te dice su equivalencia en horas y minutos
   Uso: minutosHoras 128 = 2 horas y 8 minutos
-}

minutosHoras :: Int -> String 
minutosHoras minutos = show (div minutos 60) ++  " horas y " ++ show (mod minutos 60)  ++ " minutos"

{- Funcion: esEstafa
   Descripcion: Detectar si hubo una estafa  a la hora de pagar en una tienda
   Uso: esEstafa 100 200 100 0 = true
-}

esEstafa :: Int -> Int -> Int -> Int -> Bool
esEstafa x y z w = if y - x == z && z > w then True else False

{- Funcion: esDescendente
   Descripcion: Evaluar si una lista de 4 elementos es descendente
   Uso: esDescendente 
-}

esDescendente :: Int -> Int -> Int -> Int -> Bool
esDescendente x y z w = if x > y && y > z && z > w then True
else False

{- Funcion: imc
   Descripcion: Calcular el IMC de una persona con base en su peso y altura
   Uso:imc imc 53.5 161 = normal
-}

imc :: Float -> Float -> String
imc peso altura =  if  ( peso / ((altura / 100) * (altura / 100))) <= 18.5 then "bajo"
else if ( peso / ((altura / 100) * (altura / 100))) > 18.5 && ( peso / ((altura / 100) * (altura / 100))) <= 24.9 then "normal"
else if ( peso / ((altura / 100) * (altura / 100))) > 25.0 && ( peso / ((altura / 100) * (altura / 100))) <= 29.9 then "sobrepeso"
else "obesidad"

{- Funcion: hipotenusa
   Descripcion: calcular la hipotenusa de un triangulo rectangulo (No pasa nada si se ingresa alrevez, de todos modos es una funcion simetrica)
   Uso: hipotenusa 9.0 12.0 = 15.0
-}

hipotenusa:: Float -> Float -> Float
hipotenusa b h = sqrt ( (b * b) + (h * h) )

{- Funcion: pendiente
   Descripcion: dar el valor de la pendiente que pasa por 2 puntos
   Uso: pendiente (3.0 , 2.0) (7.0 ,8.0) = 15.0
-}

pendiente :: (Float, Float) -> (Float, Float) -> Float
pendiente (x1, y1) (x2, y2) = (y2 - y1) /(x2 - x1)

{- Funcion: distanciaPuntos
   Descripcion: Nos devuelve el valor de la dstancia de una pendiente de dos puntos
   Uso: distanciaPuntos (2.0 , 1.0) (5.0 , 5.0) = 5.0
-}

distanciaPuntos :: (Float, Float) -> (Float, Float) -> Float
distanciaPuntos (x1, y1) (x2, y2) = sqrt (((x2 - x1) * (x2 - x1)) + ((y2 - y1) * (y2 - y1)))
