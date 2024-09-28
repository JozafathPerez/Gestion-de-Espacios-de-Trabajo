{-# LANGUAGE DeriveGeneric #-}

module Reservas (leerReservas,imprimirCodigosReservas, crearReserva, Reserva) where

--Dependencias
import System.IO (hFlush, stdout)
import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import Data.List (find, isPrefixOf)
import Data.Maybe (isNothing, fromJust, mapMaybe)
import Data.List.Split (splitOn)
import GHC.Generics (Generic)
import Data.Time (Day, parseTimeM, defaultTimeLocale)

import SalaReuniones (salaPorCodigo, leerNumeroCodigo, SalaReuniones(..))
import Usuarios (leerUsuarios, validarUsuario, Usuario(..))

--Definición de reserva
data Reserva = Reserva
  { codigoReserva :: String
  , codigoSalaR    :: String
  , codigoUsuario :: String
  , fecha         :: Day      
  , cantPersonas  :: Int
  } deriving (Show, Generic)

-- Instancias de FromJSON y ToJSON
instance FromJSON Reserva
instance ToJSON Reserva

-- Función para leer reservas desde un archivo JSON
leerReservas :: FilePath -> IO (Either String [Reserva])
leerReservas path = do
    contenido <- B.readFile path
    let reservas = eitherDecode contenido :: Either String [Reserva]
    case reservas of
        Left err -> do
            putStrLn ("Error al leer las reservas: " ++ err)
            return (Left err)
        Right reservasList -> return (Right reservasList)

-- Función para imprimir todos los códigos de reserva
imprimirCodigosReservas :: [Reserva] -> IO ()
imprimirCodigosReservas reservas = do
    putStrLn "Códigos de reserva:"
    mapM_ (putStrLn . codigoReserva) reservas

-- Función para guardar reservas en un archivo JSON
guardarReservas :: FilePath -> [Reserva] -> IO ()
guardarReservas path reservas = B.writeFile path (encode reservas)


-- Generar codigo de reserva
generarCodigoReserva :: [Reserva] -> String
generarCodigoReserva reservas =
    let codigosExistentes = map codigoReserva reservas
        numeros = mapMaybe (leerNumeroCodigo "RESERVA_") codigosExistentes
        nuevoNumero = if null numeros then 1 else maximum numeros + 1
    in "RESERVA_" ++ show nuevoNumero

-- Función para pedir un código de sala y validarlo
pedirCodigoSala :: FilePath -> IO SalaReuniones
pedirCodigoSala archivoSalas = do
    putStr "Ingrese el código de la sala: "
    hFlush stdout
    codigo <- getLine
    
    sala <- salaPorCodigo archivoSalas codigo
    
    case sala of
        Just salaValida -> return salaValida
        Nothing -> do
            putStrLn "Sala no encontrada. Por favor, ingrese un código válido."
            pedirCodigoSala archivoSalas -- Volver a pedir el código si no se encuentra la sala

-- Función auxiliar para validar el código de un usuario
-- pedirCodigoUsuario :: FilePath -> IO String
-- pedirCod

-- Función auxiliar para validar una fecha ingresada por el usuario
pedirFecha :: IO Day
pedirFecha = do
    putStr "Ingrese la fecha de la reserva (dd/mm/yyyy): "
    hFlush stdout
    fechaStr <- getLine
    let formato = "%d/%m/%Y"
    case parseTimeM True defaultTimeLocale formato fechaStr of
        Just fechaValida -> return fechaValida
        Nothing -> do
            putStrLn "Fecha inválida. Por favor, ingrese una fecha válida."
            pedirFecha -- Volver a pedir la fecha hasta que sea válida

-- Función para verificar si ya existe una reserva con el mismo código de sala y fecha

existeReserva :: [Reserva] -> String -> Day -> Bool
existeReserva reservas codigoSala fechaBuscada = 
    any (\r -> codigoSalaR r == codigoSala && fecha r == fechaBuscada) reservas



-- Función para pedir la cantidad de personas y validarla
pedirCantPersonas :: Int -> IO Int
pedirCantPersonas capacidad = do
    putStr "Ingrese la cantidad de personas: "
    hFlush stdout
    cantPersonasStr <- getLine

     -- Intentar leer la cantidad de personas como un entero
    case reads cantPersonasStr :: [(Int, String)] of
        [(cantPersonas, "")] ->  -- Se pudo leer un número
            if cantPersonas <= capacidad then
                return cantPersonas  -- Cantidad válida, devuelve el número
            else do
                putStrLn $ "La cantidad de personas no puede exceder la capacidad de la sala (" ++ show capacidad ++ ")."
                pedirCantPersonas capacidad  -- Volver a pedir la cantidad si excede la capacidad
        _ -> do
            putStrLn "Entrada inválida. Por favor, ingrese un número entero."
            pedirCantPersonas capacidad  -- Volver a pedir la cantidad si la entrada no es un número

crearReserva :: [Reserva] -> IO Reserva
crearReserva reservasExistentes = do
    -- Pedir y validar el código de la sala
    sala <- pedirCodigoSala "salas.json"
    let codigoS = codigoSala sala

    putStr "Ingrese el código del usuario: "
    hFlush stdout
    codigoU <- getLine

    -- Pedir y validar la fecha
    fechaValida <- pedirFecha

    -- Verificar si ya existe una reserva con el mismo código de sala y fecha
    if existeReserva reservasExistentes codigoS fechaValida
        then do
            putStrLn "La fecha no está disponible para la sala seleccionada. Por favor, elija otra fecha."
            crearReserva reservasExistentes  -- Volver a crear la reserva
        else do
            -- Pedir y validar la cantidad de personas
            cantidadPersonas <- pedirCantPersonas (capacidad sala)

            -- Generar el código de la reserva
            let codigoR = generarCodigoReserva reservasExistentes

            -- Crear la estructura de Reserva con los datos ingresados
            return Reserva {
                codigoReserva = codigoR,
                codigoSalaR = codigoS,
                codigoUsuario = codigoU,
                fecha = fechaValida,
                cantPersonas = cantidadPersonas
            }

    

-- Crear reserva



