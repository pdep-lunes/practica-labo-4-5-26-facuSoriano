data Perro = UnPerro {
    raza :: String,
    juguetesFav :: [String],
    tiempo :: Int,
    energia :: Int
} deriving (Show, Eq)

cambiarEnergia :: (Int -> Int) -> Perro -> Perro
cambiarEnergia unaFuncion unPerro = unPerro{energia = max 0 . unaFuncion . energia $ unPerro }

jugar :: Perro -> Perro
jugar  unPerro = cambiarEnergia (subtract 10) unPerro

ladrar :: Int -> Perro -> Perro
ladrar ladridos unPerro = cambiarEnergia (+ div ladridos 2) unPerro

regalar :: String -> Perro ->  Perro
regalar unJuguete unPerro = unPerro {juguetesFav = unJuguete : juguetesFav unPerro}

esExtravagante :: Perro -> Bool
esExtravagante unPerro = raza unPerro == "dalmata" || raza unPerro == "pomerania"

fiftyMins :: Perro -> Bool
fiftyMins unPerro = tiempo unPerro >= 50

sacarJuguete ::Perro -> Perro
sacarJuguete unPerro = unPerro { juguetesFav = drop 1 (juguetesFav unPerro)}

diaDeCampo :: Perro -> Perro
diaDeCampo unPerro =   jugar . sacarJuguete  $ unPerro

diaDeSpa :: Perro -> Perro
diaDeSpa unPerro
    | fiftyMins unPerro || esExtravagante unPerro = cambiarEnergia (const 100) . regalar "peine de goma" $ unPerro
    | otherwise = unPerro

zara :: Perro
zara = UnPerro "dalmata" ["pelota", "mantita"] 90 80

type Ejercicio = Perro -> Perro

type Actividad = [(Ejercicio, Int)]

data Guarderia = UnaGuarderia {
    nombre :: String,
    rutina :: Actividad
} 

guarderiaPdePerritos :: Actividad
guarderiaPdePerritos =
    [ (jugar, 30)
    , (ladrar 18, 20)
    , (regalar "pelota", 0)
    , (diaDeSpa, 120)
    , (diaDeCampo, 720)
    ] 

duracionRutina :: Actividad -> Int
duracionRutina = sum . map snd

puedeEstarEnGuarderia :: Perro -> Actividad -> Bool
puedeEstarEnGuarderia unPerro rutina = tiempo unPerro > duracionRutina rutina

esResponsable :: Perro -> Bool
esResponsable unPerro = length(juguetesFav (diaDeCampo unPerro)) >3

--Que un perro realice una rutina de la guardería, revisando antes que el tiempo de la rutina no puede ser mayor al tiempo
--de permanencia.
--Dados unos perros, reportar todos los que quedan cansados después de realizar la rutina de una guardería.

realizarRutina :: Actividad -> Perro -> Perro
realizarRutina rutina unPerro
    | puedeEstarEnGuarderia unPerro rutina = foldl aplicarEjercicio unPerro rutina
    | otherwise = unPerro

aplicarEjercicio :: Perro -> (Ejercicio, int) -> Perro
aplicarEjercicio unPerro (ejercicio, _) = ejercicio unPerro

estaCansado :: Perro -> Bool
estaCansado unPerro = energia unPerro == 0

perrosCansados :: Actividad -> [Perro] -> [Perro]
perrosCansados rutina = filter (estaCansado . realizarRutina rutina)

