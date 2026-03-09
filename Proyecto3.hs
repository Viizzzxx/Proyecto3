import System.IO

-- Funciones de conversión
celsiusAFahrenheit :: Float -> Maybe Float
celsiusAFahrenheit c
    | c >= -273.15 = Just ((c * 9 / 5) + 32)  -- Cero absoluto en Celsius
    | otherwise = Nothing

fahrenheitACelsius :: Float -> Maybe Float
fahrenheitACelsius f
    | f >= -459.67 = Just ((f - 32) * 5 / 9)  -- Cero absoluto en Fahrenheit
    | otherwise = Nothing

metrosAPies :: Float -> Maybe Float
metrosAPies m
    | m >= 0 = Just (m * 3.28084)
    | otherwise = Nothing

piesAMetros :: Float -> Maybe Float
piesAMetros p
    | p >= 0 = Just (p / 3.28084)
    | otherwise = Nothing

-- Menú principal
main :: IO ()
main = do
    putStrLn "Seleccione una opción:"
    putStrLn "1. Convertir Celsius a Fahrenheit"
    putStrLn "2. Convertir Fahrenheit a Celsius"
    putStrLn "3. Convertir Metros a Pies"
    putStrLn "4. Convertir Pies a Metros"
    putStrLn "5. Salir"
    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "Ingrese grados Celsius:"
            celsius <- readLn
            case celsiusAFahrenheit celsius of
                Just f -> putStrLn $ "Fahrenheit: " ++ show f
                Nothing -> putStrLn "Error: Temperatura por debajo del cero absoluto."
            main
        "2" -> do
            putStrLn "Ingrese grados Fahrenheit:"
            fahrenheit <- readLn
            case fahrenheitACelsius fahrenheit of
                Just c -> putStrLn $ "Celsius: " ++ show c
                Nothing -> putStrLn "Error: Temperatura por debajo del cero absoluto."
            main
        "3" -> do
            putStrLn "Ingrese metros:"
            metros <- readLn
            case metrosAPies metros of
                Just p -> putStrLn $ "Pies: " ++ show p
                Nothing -> putStrLn "Error: Longitud negativa no permitida."
            main
        "4" -> do
            putStrLn "Ingrese pies:"
            pies <- readLn
            case piesAMetros pies of
                Just m -> putStrLn $ "Metros: " ++ show m
                Nothing -> putStrLn "Error: Longitud negativa no permitida."
            main
        "5" -> putStrLn "Saliendo..."
        _   -> do
            putStrLn "Opción no válida. Intente de nuevo."
            main