data Perro = UnPerro {
    raza :: String,
    juguetesFav :: [String],
    tiempo :: Int,
    energia :: Int
}

cambiarEnergia :: (Int -> Int) -> Perro -> Perro
cambiarEnergia unaFuncion unPerro = unPerro{energia = max 0 . unaFuncion . energia $ unPerro }

jugar :: Perro -> Perro
jugar  unPerro = cambiarEnergia (subtract 10) unPerro

ladrar :: Perro -> Int -> Perro
ladrar unPerro ladridos = cambiarEnergia (+ div ladridos 2) unPerro

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

