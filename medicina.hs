import Distribution.PackageDescription (BuildInfo(otherExtensions))
data Animal= Raton {nombre :: String, edad :: Double, peso :: Double,
 enfermedades :: [String]} deriving Show
-- Ejemplo de raton
cerebro :: Animal
cerebro = Raton "Cerebro" 9.0 0.2 ["brucelosis", "sarampión", "tuberculosis"]
orejudo :: Animal
orejudo = Raton "Orejudo" 4.0 10.0 ["obesidad", "sinusitis"]
-- Estos son las enfermedades infecciosas
enfermedadesInfecciosas = [ "brucelosis", "tuberculosis"]

modificarNombre :: (String -> String ) -> Animal -> Animal
modificarNombre f animal = animal { nombre = (f.nombre) animal}

modificarEdad :: ( Double -> Double) -> Animal -> Animal
modificarEdad f animal = animal {edad = (f.edad) animal}

modificarPeso :: (Double -> Double) -> Animal -> Animal
modificarPeso f animal = animal {peso = f.peso $ animal}

modificarEnfermedades :: ([String]-> [String]) -> Animal -> Animal
modificarEnfermedades f raton = raton { enfermedades  = f.enfermedades $ raton}

hierbaBuena :: Animal -> Animal
hierbaBuena raton = modificarEdad sqrt raton

hierbaVerde :: String -> Animal -> Animal
hierbaVerde enfermedad raton = modificarEnfermedades (filter (/= enfermedad)) raton


alcachofa :: Animal -> Animal
alcachofa raton = modificarPeso perderPeso raton

perderPeso :: Double -> Double
perderPeso peso | peso > 2 = peso * 0.9  
                 | otherwise = peso * 0.95

hierbaMagica :: Animal -> Animal
hierbaMagica unAnimal = modificarEdad (0*).modificarEnfermedades (const [])  $ unAnimal

medicamento :: [(Animal -> Animal)] -> Animal -> Animal
medicamento hierbas raton = foldl  (flip ($)) raton  hierbas

medicamento' :: [(Animal -> Animal)] -> Animal -> Animal
medicamento' hierbas raton = foldl (\unRaton unaHierba -> unaHierba unRaton)  raton hierbas

antiAge :: Animal -> Animal
antiAge raton = medicamento (replicate 3 hierbaBuena ++ [alcachofa]) raton

reduceFatFast  :: Int -> Animal -> Animal
reduceFatFast nro raton = medicamento ([hierbaVerde "obesidad"]++ ( replicate nro alcachofa)) raton

hierbaMilagrosa :: Animal -> Animal
hierbaMilagrosa raton = medicamento (map hierbaVerde enfermedadesInfecciosas) raton

cantidadIdeal :: (Int -> Bool) -> Int
cantidadIdeal criterio = head . filter criterio $ [1..] 


estanMejoresQueNunca :: [Animal] -> (Animal -> Animal) -> Bool
estanMejoresQueNunca  ratones unMedicamento =   all ((<1).peso.unMedicamento) ratones


potenciaIdeal :: [Animal] -> Int
potenciaIdeal ratones = cantidadIdeal (estanMejoresQueNunca ratones.reduceFatFast)