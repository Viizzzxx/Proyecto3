module Main where

data Unidad = Celsius | Fahrenheit | Metros | Pies
    deriving (Show, Eq)

convertir :: Unidad -> Unidad -> Float -> Maybe Float
-- Temperatura
convertir Celsius Fahrenheit x = Just ((x * 9 / 5) + 32)
convertir Fahrenheit Celsius x = Just ((x - 32) * 5 / 9)

-- Longitud
convertir Metros Pies x = Just (x * 3.28084)
convertir Pies Metros x = Just (x / 3.28084)

-- Misma unidad
convertir Celsius Celsius x = Just x
convertir Fahrenheit Fahrenheit x = Just x
convertir Metros Metros x = Just x
convertir Pies Pies x = Just x

-- Conversiones no válidas
convertir _ _ _ = Nothing

main :: IO ()
main = do
    putStrLn "=== Conversor de Unidades ==="
    putStrLn "1. Celsius a Fahrenheit"
    putStrLn "2. Fahrenheit a Celsius"
    putStrLn "3. Metros a Pies"
    putStrLn "4. Pies a Metros"
    putStrLn "Selecciona una opcion:"
    
    opcion <- getLine
    
    putStrLn "Ingresa el valor a convertir:"
    entrada <- getLine
    let valor = read entrada :: Float

    let resultado = case opcion of
            "1" -> convertir Celsius Fahrenheit valor
            "2" -> convertir Fahrenheit Celsius valor
            "3" -> convertir Metros Pies valor
            "4" -> convertir Pies Metros valor
            _   -> Nothing

    case resultado of
        Just r  -> putStrLn ("Resultado: " ++ show r)
        Nothing -> putStrLn "Error: conversion no valida."