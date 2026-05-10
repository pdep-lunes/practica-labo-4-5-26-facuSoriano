data Perro = UnPerro {
    raza :: String,
    juguetesFav :: [String],
    tiempo :: Int,
    energia :: Int
} deriving Show

cambiarEnergia :: (Int -> Int) -> Perro -> Perro
cambiarEnergia unaFuncion unPerro = unPerro{energia = max 0 . unaFuncion . energia $ unPerro }

jugar :: Perro -> Perro
jugar  unPerro = cambiarEnergia (subtract 10) unPerro

ladrar :: Perro -> Int -> Perro
ladrar unPerro ladridos = cambiarEnergia (+ (div ladridos 2)) unPerro

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

type Actividad = Perro -> Perro

type Rutina = [(Actividad, Int)]

guarderiaPdePerritos :: Rutina
guarderiaPdePerritos =
    [ (jugar, 30)
    , (ladrar 18, 20)
    , (regalar "pelota", 0)
    , (diaDeSpa, 120)
    , (diaDeCampo, 720)
    ]

duracionRutina :: Rutina -> Int
duracionRutina = sum . map snd

puedeEstarEnGuarderia :: Perro -> Rutina -> Bool
puedeEstarEnGuarderia unPerro rutina = tiempo unPerro > duracionRutina rutina

esResponsable :: Perro -> Bool
esResponsable unPerro = length(juguetesFav (diaDeCampo unPerro)) >3