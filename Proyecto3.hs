import System.IO

-- Funciones de conversión
celsiusAFahrenheit :: Float -> Float
celsiusAFahrenheit c = (c * 9 / 5) + 32

fahrenheitACelsius :: Float -> Float
fahrenheitACelsius f = (f - 32) * 5 / 9

metrosAPies :: Float -> Float
metrosAPies m = m * 3.28084

piesAMetros :: Float -> Float
piesAMetros p = p / 3.28084

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
            putStrLn $ "Fahrenheit: " ++ show (celsiusAFahrenheit celsius)
            main
        "2" -> do
            putStrLn "Ingrese grados Fahrenheit:"
            fahrenheit <- readLn
            putStrLn $ "Celsius: " ++ show (fahrenheitACelsius fahrenheit)
            main
        "3" -> do
            putStrLn "Ingrese metros:"
            metros <- readLn
            putStrLn $ "Pies: " ++ show (metrosAPies metros)
            main
        "4" -> do
            putStrLn "Ingrese pies:"
            pies <- readLn
            putStrLn $ "Metros: " ++ show (piesAMetros pies)
            main
        "5" -> putStrLn "Saliendo..."
        _   -> do
            putStrLn "Opción no válida. Intente de nuevo."
            main
