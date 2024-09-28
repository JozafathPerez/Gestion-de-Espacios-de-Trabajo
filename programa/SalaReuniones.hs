{-# LANGUAGE DeriveGeneric #-}

module SalaReuniones (crearYMostrarSala, mostrarSalaPorCodigo, salaPorCodigo,leerSalas, leerNumeroCodigo, SalaReuniones(..)) where

import System.IO (hFlush, stdout)
import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import Data.List (find, isPrefixOf)
import Data.Maybe (isNothing, fromJust, mapMaybe)
import Data.List.Split (splitOn)
import Mobiliario (Mobiliario(..), leerMobiliario)
import GHC.Generics (Generic)

-- Definición de Sala de Reuniones
data SalaReuniones = SalaReuniones
  { codigoSala :: String
  , nombreSala :: String
  , edificio   :: String
  , piso       :: String  
  , ubicacion  :: String
  , capacidad  :: Int
  , mobiliario :: [Mobiliario]
  } deriving (Show, Generic)

-- Instancias de FromJSON y ToJSON
instance FromJSON SalaReuniones
instance ToJSON SalaReuniones

-- Leer las salas desde un archivo JSON
leerSalas :: FilePath -> IO (Either String [SalaReuniones])
leerSalas path = do
    contenido <- B.readFile path
    let salas = eitherDecode contenido :: Either String [SalaReuniones]
    case salas of
        Left err -> do
            putStrLn ("Error al leer las salas: " ++ err)
            return (Left err)
        Right salasList -> return (Right salasList)

-- Función para guardar las salas en un archivo JSON
guardarSalas :: FilePath -> [SalaReuniones] -> IO ()
guardarSalas path salas = B.writeFile path (encode salas)

-- Generar un código único para una nueva sala basado en los códigos existentes
generarCodigoSala :: [SalaReuniones] -> String
generarCodigoSala salas =
    let codigosExistentes = map codigoSala salas
        numeros = mapMaybe (leerNumeroCodigo "SALA_") codigosExistentes
        nuevoNumero = if null numeros then 1 else maximum numeros + 1
    in "SALA_" ++ show nuevoNumero

-- Función auxiliar para extraer el número de un código de sala
leerNumeroCodigo :: String -> String -> Maybe Int
leerNumeroCodigo prefijo codigo =
    if prefijo `isPrefixOf` codigo
    then Just (read (drop (length prefijo) codigo) :: Int)
    else Nothing

-- Crear una nueva sala de reuniones
crearSala :: [SalaReuniones] -> [Mobiliario] -> IO SalaReuniones
crearSala salasExistentes mobiliarioExistente = do
    -- Pedir al usuario la información básica de la sala
    putStr "Ingrese el nombre de la sala: "
    hFlush stdout
    nombre <- getLine

    putStr "Ingrese el edificio: "
    hFlush stdout
    edificio <- getLine

    putStr "Ingrese el piso: "
    hFlush stdout
    piso <- getLine

    putStr "Ingrese la ubicación: "
    hFlush stdout
    ubicacion <- getLine

    putStr "Ingrese la capacidad: "
    hFlush stdout
    capacidadStr <- getLine
    let capacidad = read capacidadStr :: Int

    -- Mostrar mobiliario disponible
    putStrLn "Seleccione el mobiliario que posee la sala (separado por comas, ej: 1,2,3): "
    mostrarMobiliarioDisponibles mobiliarioExistente

    -- Selección de mobiliario por parte del usuario
    seleccionStr <- getLine
    let indicesSeleccionados = map read (splitOn "," seleccionStr) :: [Int]
    let mobiliarioSeleccionado = [mobiliarioExistente !! (i - 1) | i <- indicesSeleccionados]

    -- Generar código de sala
    let codigo = generarCodigoSala salasExistentes

    -- Devolver la nueva sala creada
    return SalaReuniones {
        codigoSala = codigo,
        nombreSala = nombre,
        edificio = edificio,
        piso = piso,
        ubicacion = ubicacion,
        capacidad = capacidad,
        mobiliario = mobiliarioSeleccionado
    }

-- Mostrar el mobiliario disponible
mostrarMobiliarioDisponibles :: [Mobiliario] -> IO ()
mostrarMobiliarioDisponibles mobiliario = do
    putStrLn "+-------------------------------------+"
    putStrLn "|     Mobiliario Disponible           |"
    putStrLn "+-------------------------------------+"
    mapM_ mostrarItem (zip [1..] mobiliario)
  where
    mostrarItem (i, m) = putStrLn $ show i ++ ". " ++ nombre m ++ " (" ++ tipo m ++ ")"

-- Función para crear una sala y guardarla en el sistema
crearYMostrarSala :: FilePath -> FilePath -> IO ()
crearYMostrarSala archivoMobiliario archivoSalas = do
    -- Leer mobiliario disponible
    mobiliario <- leerMobiliario archivoMobiliario

    -- Leer las salas existentes desde la base de datos
    salasExistentes <- leerSalas archivoSalas

    case salasExistentes of
        Left _ -> do
            salaNueva <- crearSala [] mobiliario
            guardarSalas archivoSalas [salaNueva]
            putStrLn "Sala de reuniones creada con éxito!"
            mostrarSala salaNueva
        Right salas -> do
            salaNueva <- crearSala salas mobiliario
            let salasActualizadas = salas ++ [salaNueva]
            guardarSalas archivoSalas salasActualizadas
            putStrLn "Sala de reuniones creada con éxito!"
            mostrarSala salaNueva

-- Mostrar información de una sala
mostrarSala :: SalaReuniones -> IO ()
mostrarSala sala = do
    putStrLn "+-------------------------------------+"
    putStrLn "|          Información de Sala        |"
    putStrLn "+-------------------------------------+"
    putStrLn $ "Código: " ++ codigoSala sala
    putStrLn $ "Nombre: " ++ nombreSala sala
    putStrLn $ "Edificio: " ++ edificio sala
    putStrLn $ "Piso: " ++ piso sala
    putStrLn $ "Ubicación: " ++ ubicacion sala
    putStrLn $ "Capacidad: " ++ show (capacidad sala)
    putStrLn "+-------------------------------------+"
    putStrLn "|          Mobiliario de Sala         |"
    putStrLn "+-------------------------------------+"
    mapM_ mostrarMobiliario (mobiliario sala)
  where
    mostrarMobiliario m = putStrLn $ " - " ++ nombre m ++ " (" ++ tipo m ++ ")"

-- Buscar y mostrar una sala por código
mostrarSalaPorCodigo :: FilePath -> String -> IO ()
mostrarSalaPorCodigo archivoSalas codigo = do
    salas <- leerSalas archivoSalas
    case salas of
        Left err -> putStrLn ("Error al leer las salas: " ++ err)
        Right salasList -> do
            let salaEncontrada = find (\s -> codigoSala s == codigo) salasList
            case salaEncontrada of
                Just sala -> mostrarSala sala
                Nothing -> putStrLn "Sala no encontrada."

-- Buscar y devolver una sala por código
salaPorCodigo :: FilePath -> String -> IO (Maybe SalaReuniones)
salaPorCodigo archivoSalas codigo = do
    salas <- leerSalas archivoSalas
    case salas of
        Left err -> do
            putStrLn ("Error al leer las salas: " ++ err)
            return Nothing
        Right salasList -> do
            let salaEncontrada = find (\s -> codigoSala s == codigo) salasList
            return salaEncontrada