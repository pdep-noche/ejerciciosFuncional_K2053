import Text.Show.Functions

data Personaje = Personaje {nombre :: String, experiencia :: Float, fuerza :: Float, elemento:: Elemento} deriving Show

type Elemento = Float -> Float

--nivel :: Personaje -> Float
nivel (Personaje _ experiencia _ _) = ceiling(experiencia^2 /(experiencia + 1))

capacidad :: Personaje -> Float
capacidad (Personaje _ _ fuerza elemento) = elemento fuerza

espadaOxidada = (1.2*)
katanaFilosa = (10+).(0.9*)
sableLambdico cm = ((1+cm/100)*)
redParadigmatica = sqrt
baculoDuplicador x= x* 2
espadaMaldita = espadaOxidada.sableLambdico 89


cecilia = Personaje "cecilia" 40 50 baculoDuplicador
pedro = Personaje "pedro" 50 30 (sableLambdico 30)

type Alquimista = Personaje -> Personaje

aprendiz :: Alquimista
aprendiz personaje = alterarElemento (2*) personaje

alterarElemento :: Elemento -> Alquimista
alterarElemento f personaje = personaje { elemento = f.elemento personaje}

maestroAlquimista :: Int -> Alquimista
maestroAlquimista años personaje = alterarElemento (extraPorAntiguedad años).aprendiz $ personaje

extraPorAntiguedad 0 = id
extraPorAntiguedad años = (*1.1) .extraPorAntiguedad (años -1)

estafador :: Alquimista
estafador personaje = personaje { elemento = id}

inventado :: Alquimista
inventado personaje = personaje {elemento = (\nro-> nro + (experiencia personaje))}

capacidadesSuperioresA :: Float -> Personaje -> [Alquimista] -> [Alquimista]
capacidadesSuperioresA valor personaje alquimistas = filter (tieneCapacidadSuperiorA valor personaje)  alquimistas

tieneCapacidadSuperiorA :: Float -> Personaje -> Alquimista -> Bool
tieneCapacidadSuperiorA valor personaje alquimista =  (>valor).capacidad.alquimista $ personaje


convieneTodos :: Personaje ->[Alquimista] -> Bool
convieneTodos personaje alquimistas = all (tieneCapacidadSuperiorA (capacidad personaje) personaje) alquimistas


{-
ghci> convieneTodos pedro [aprendiz, (maestroAlquimista 3)] 
True 
-}

data Monstruo = Monstruo {especie :: String, resistencia :: Float, habilidades :: [Habilidad]} deriving Show
type Habilidad = (String, String)


esAgresivo :: Monstruo -> Bool
esAgresivo  monstruo = (tieneMayoriaHabilidadesOfensivas. habilidades) monstruo && ((>0).resistencia) monstruo && (not.especieInofensiva.especie) monstruo

especieInofensiva :: String -> Bool
especieInofensiva especie = elem especie [ "animal", "Chocobo"]

descripcion = fst
tipo = snd
tieneMayoriaHabilidadesOfensivas :: [Habilidad] -> Bool
tieneMayoriaHabilidadesOfensivas habilidades = (length.filter(esOfensiva.tipo)) habilidades > div (length habilidades) 2


esOfensiva:: String -> Bool
esOfensiva "magia" = True
esOfensiva "fisica" = True
esOfensiva _ = False


leGana :: Personaje -> Monstruo -> Bool
leGana personaje monstruo = capacidad personaje > resistencia monstruo

pelearConTodos :: Personaje -> [Monstruo] -> Personaje
pelearConTodos personaje monstruos = foldl pelear personaje monstruos

pelear :: Personaje -> Monstruo -> Personaje
pelear personaje monstruo | leGana personaje monstruo = modificarExperiencia 100 personaje
                          | otherwise = alterarElemento (*0.9).modificarExperiencia (-50) $ personaje


modificarExperiencia :: Float -> Personaje -> Personaje
modificarExperiencia valor personaje = personaje { experiencia = experiencia personaje + valor}


pierdeConAlguno :: Personaje -> Alquimista -> [Monstruo] -> Bool
pierdeConAlguno personaje alquimista monstruos = any (not.leGana (alquimista personaje)) monstruos