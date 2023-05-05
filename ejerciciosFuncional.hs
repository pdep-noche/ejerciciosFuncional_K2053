find' :: (a -> Bool) -> [a] -> a
find' criterio lista = (head . filter criterio) lista 

data Politico = Politico {proyectosPresentados :: [String], sueldo :: Integer,  edad :: Int } deriving Show 
politicos = [ Politico ["ser libres", "libre estacionamiento coches politicos", "ley no fumar", "ley 19182"] 20000 81, Politico ["tratar de reconquistar luchas sociales"] 10000 63, Politico ["tolerancia 100 para delitos"] 15500 49 ]

pedro :: Politico
pedro = Politico ["tolerancia 100 para delitos"] 15500 49 

{- a
ghci> find' ((<50).edad)  politicos
Politico {proyectosPresentados = ["tolerancia 100 para delitos"], sueldo = 15500, edad = 49}
-}

{- b
ghci> find' ((>3).length.proyectosPresentados) politicos
Politico {proyectosPresentados = ["ser libres","libre estacionamiento coches politicos","ley no fumar","ley 
19182"], sueldo = 20000, edad = 81}
-}

{- c
ghci> find' (any ((>3).length.words).proyectosPresentados) politicos
Politico {proyectosPresentados = ["ser libres","libre estacionamiento coches politicos","ley no fumar","ley 
19182"], sueldo = 20000, edad = 81}
-}

type Nombre = String
type Notas = [Int]
data Persona = Alumno {nombre :: Nombre, notas :: Notas}


promediosAlumnos :: [Persona] -> [(Nombre, Int)]
promediosAlumnos alumnos = map (\unAlumno -> (nombre unAlumno,(promedio.notas)unAlumno)) alumnos

promedio :: Notas -> Int
promedio notas = (sum notas) `div` (length notas)

{-
ghci> promediosAlumnos[(Alumno "juan" [8,6]), (Alumno "maria" [7,9,4]), (Alumno "ana" [6,2,4])]
[("juan",7),("maria",6),("ana",4)]
-}

promediosSinAplazos :: [Notas] -> [Int]
promediosSinAplazos  listaNotas = map (promedio.filter (>=6)) listaNotas

aprobo :: Persona -> Bool
aprobo (Alumno _ notas) = all (>=6) notas

aprobaron :: [Persona] -> [Nombre]
aprobaron alumnos = (map nombre.filter aprobo) alumnos

productos :: [Nombre] -> [Integer] -> [(Nombre, Integer)]
productos nombres precios = zip nombres precios

productos' :: [Nombre] -> [Integer] -> [(Nombre, Integer)]
productos' nombres precios = zipWith (\nom prec -> (nom, prec)) nombres precios