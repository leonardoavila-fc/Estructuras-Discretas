reconversion :: Float -> Float
reconversion dinero = dinero / 1000

cashback :: Float -> Float
cashback monto = monto / 10

cashbackMonto :: Float -> Float -> Float
cashbackMonto puntos valorPuntos = puntos * valorPuntos

minutosHoras :: Int -> String 
minutosHoras minutos = show (div minutos 60) ++  " horas y " ++ show (mod minutos 60)  ++ " minutos"

esEstafa :: Int -> Int -> Int -> Int -> Bool
esEstafa valor billeteTrampa billeteDeTroya pagoDelMercancia = if - valor + billeteTrampa - billeteDeTroya + pagoDelMercancia == 0 then True
else False

esDescendente :: Int -> Int -> Int -> Int -> Bool
esDescendente x y z w = if x > y && y > z && z > w then True
else False