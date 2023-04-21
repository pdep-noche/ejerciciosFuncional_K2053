import Control.Concurrent.STM.TChan (newBroadcastTChan)
siguiente :: Integer -> Integer
siguiente nro = nro + 1

--calcular :: Integer -> Integer
calcular nro | even nro = siguiente nro
             | otherwise = doble nro

doble nro = nro * 2

aproboAlumno :: Integer -> Bool
aproboAlumno nota = nota >= 6

calcular' :: (Integer, Integer) -> (Integer, Integer)
calcular' (primero, segundo) = (duplicaPar primero, sumarUnoImpar segundo)

duplicaPar :: Integer -> Integer
duplicaPar nro | even nro = doble nro
               | otherwise = nro

sumarUnoImpar :: Integer -> Integer
sumarUnoImpar nro | odd nro = siguiente nro
                   | otherwise = nro

and' :: Bool -> Bool -> Bool
and' valor1 valor2 |not valor1 = False      
                   | not valor2 = False
                   | otherwise = True

and'' :: Bool -> Bool -> Bool
and'' valor otroValor | valor = otroValor
                      | otherwise = False


and''' :: Bool -> Bool -> Bool
and''' True otroValor = otroValor
and'''  _  otroValor = False

or' :: Bool -> Bool -> Bool
or' False False = False
or' _ _ = True

or'' :: Bool -> Bool -> Bool
or'' False otroValor = otroValor
or'' _ _ = True

or''' v1 v2 | v1 = True
            |otherwise = v2

or'''' True _ = True
or'''' _ True = True
or'''' _ _  = False

type Nota = Integer
type Alumno = (String, Nota, Nota, Nota)


notaMaxima :: Alumno -> Nota
notaMaxima (_, nota, otraNota, otraNota') = nota `max` (otraNota `max` otraNota')


notaMaxima' :: Alumno -> Nota
notaMaxima' (_, nota, otraNota, otraNota') = max nota (otraNota `max` otraNota')


cuadruple :: Integer -> Integer
cuadruple nro = doble (doble nro)



