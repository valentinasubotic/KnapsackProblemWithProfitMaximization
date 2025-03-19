import Data.Array

data Predmet = Predmet { cijena :: Int, bodovi :: Int } deriving (Show)

-- Maksimizacija profita implementirana dinamickim programiranjem 
maksimizacijaProfita :: Int -> [Predmet] -> (Int, [Predmet])
maksimizacijaProfita budzet predmeti =
    let n = length predmeti
        dp = array ((0,0), (n, budzet)) 
             [((i, j), opt i j) | i <- [0..n], j <- [0..budzet]]

        opt 0 _ = (0, [])  
        opt i j 
            | cijena (predmeti !! (i-1)) > j = dp ! (i-1, j)  
            | otherwise =
                let (bezPredmeta, listaBez) = dp ! (i-1, j)
                    (saPredmetom, listaSa) = dp ! (i-1, j - cijena (predmeti !! (i-1))) 
                    saPredmetom' = saPredmetom + bodovi (predmeti !! (i-1))
                in if bezPredmeta > saPredmetom'
                   then (bezPredmeta, listaBez)
                   else (saPredmetom', predmeti !! (i-1) : listaSa)

    in dp ! (n, budzet)  -- Vracamo ukupan broj bodova i listu predmeta

predmeti :: [Predmet]
predmeti = 
    [ Predmet 10 60, Predmet 15 75, Predmet 20 90, Predmet 25 110
    , Predmet 30 130, Predmet 35 140, Predmet 40 155, Predmet 45 170
    , Predmet 50 185, Predmet 55 195, Predmet 60 210, Predmet 65 225
    , Predmet 70 240, Predmet 75 260, Predmet 80 280
    ]

unosBudzeta :: IO Int
unosBudzeta = do
    putStrLn "Unesite budzet: "
    input <- getLine
    case reads input :: [(Int, String)] of
        [(budzet, "")] | budzet > 0 -> return budzet  
        _ -> do
            putStrLn "Nepravilan unos! Molimo unesite pozitivan broj."
            unosBudzeta  -- Ponovlja unos ako nije validan 

main :: IO ()
main = do
    budzet <- unosBudzeta
    let (ukupniBodovi, izabraniPredmeti) = maksimizacijaProfita budzet predmeti
    if null izabraniPredmeti
        then putStrLn "Za dati budzet ne postoji predmet koji se uklapa u cijenu."
        else do
            putStrLn $ "Ukupan broj bodova: " ++ show ukupniBodovi
            putStrLn "Izabrani predmeti:"
            mapM_ (putStrLn . show) izabraniPredmeti