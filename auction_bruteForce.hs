import Data.List (subsequences)

data Predmet = Predmet { cijena :: Int, bodovi :: Int } deriving (Show)

-- Izracunavanje ukupne cijene podskupa
ukupnaCijena :: [Predmet] -> Int
ukupnaCijena [] = 0
ukupnaCijena (x:xs) = cijena x + ukupnaCijena xs

-- Izracunavanje ukupnog broja bodova podskupa
ukupniBodovi :: [Predmet] -> Int
ukupniBodovi [] = 0
ukupniBodovi (x:xs) = bodovi x + ukupniBodovi xs

-- Filtriranje podskupova koji ne prelaze budzet
filtrirajPodskupove :: Int -> [[Predmet]] -> [[Predmet]]
filtrirajPodskupove _ [] = []
filtrirajPodskupove budzet (x:xs)
    | ukupnaCijena x <= budzet = x : filtrirajPodskupove budzet xs
    | otherwise = filtrirajPodskupove budzet xs

-- Pronalazenje podskupa sa maksimalnim brojem bodova
najboljiPodskup :: [[Predmet]] -> [Predmet]
najboljiPodskup [] = []
najboljiPodskup [x] = x
najboljiPodskup (x1:x2:xs)
    | ukupniBodovi x1 >= ukupniBodovi x2 = najboljiPodskup (x1:xs)
    | otherwise = najboljiPodskup (x2:xs)

-- Maksimizacija profita
maksimizacijaProfita :: Int -> [Predmet] -> [Predmet]
maksimizacijaProfita budzet predmeti =
    let sviPodskupovi = subsequences predmeti
        validniPodskupovi = filtrirajPodskupove budzet sviPodskupovi
    in najboljiPodskup validniPodskupovi

predmeti :: [Predmet]
predmeti = 
    [ Predmet 10 60
    , Predmet 15 75
    , Predmet 20 90
    , Predmet 25 110
    , Predmet 30 130
    , Predmet 35 140
    , Predmet 40 155
    ]

unosBudzeta :: IO Int
unosBudzeta = do
    putStrLn "Unesite budzet: "
    input <- getLine
    case reads input :: [(Int, String)] of
        [(budzet, "")] | budzet > 0 -> return budzet  
        _ -> do
            putStrLn "Nepravilan unos! Molimo unesite pozitivan broj."
            unosBudzeta 
            
main :: IO ()
main = do
    budzet <- unosBudzeta
    let rezultat = maksimizacijaProfita budzet predmeti
    if null rezultat
        then putStrLn "Za dati budzet ne postoji predmet koji se uklapa u cijenu."
        else putStrLn $ "Izabrani predmeti:\n" ++ unlines (map show rezultat)


