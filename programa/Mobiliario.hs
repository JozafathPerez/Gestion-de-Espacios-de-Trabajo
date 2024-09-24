{-# LANGUAGE DeriveGeneric #-}

module Mobiliario (Mobiliario(..), leerMobiliario, cargarYMostrarMobiliario) where

import System.IO (hFlush, stdout)
import Data.List.Split (splitOn)
import Data.Aeson (eitherDecode, encode, FromJSON, ToJSON)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import Data.List (find)
import Data.Maybe (isJust, mapMaybe)
import GHC.Generics (Generic)
import Data.Char (isSpace)

data Mobiliario = Mobiliario {
    codigo :: String,
    nombre :: String,
    descripcion :: String,
    tipo :: String  -- Puede ser consumible, electrónico o menaje
} deriving (Show, Eq, Generic)

instance FromJSON Mobiliario
instance ToJSON Mobiliario

leerMobiliario :: FilePath -> IO [Mobiliario]
leerMobiliario path = do
    contenido <- B.readFile path
    let mobiliario = eitherDecode contenido :: Either String [Mobiliario]
    case mobiliario of
        Left err -> do
            putStrLn $ "Error al leer la base de datos de mobiliario: " ++ err
            return []
        Right items -> return items

guardarMobiliario :: FilePath -> [Mobiliario] -> IO ()
guardarMobiliario path mobiliario = B.writeFile path (encode mobiliario)

cargarMobiliarioDesdeCSV :: FilePath -> IO [Mobiliario]
cargarMobiliarioDesdeCSV path = do
    contenido <- readFile path
    let lineas = lines contenido
    let datos = mapMaybe parseLineaMobiliario (tail lineas)  -- Ignorar la primera línea
    return datos

parseLineaMobiliario :: String -> Maybe Mobiliario
parseLineaMobiliario linea =
    let campos = splitOn "," linea
    in if length campos == 4
        then Just (Mobiliario (trim (campos !! 0)) (trim (campos !! 1)) (trim (campos !! 2)) (trim (campos !! 3)))
        else Nothing

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

codigoExiste :: String -> [Mobiliario] -> Bool
codigoExiste cod mobiliario = isJust $ find (\m -> codigo m == cod) mobiliario

mostrarMobiliario :: [Mobiliario] -> IO ()
mostrarMobiliario mobiliario = do
    putStrLn "+-------------------------------------+"
    putStrLn "|            Mobiliario               |"
    putStrLn "+-------------------------------------+"
    mapM_ mostrarItem mobiliario
  where
    mostrarItem m = putStrLn $ "Código: " ++ codigo m ++
                              ", Nombre: " ++ nombre m ++
                              ", Descripción: " ++ descripcion m ++
                              ", Tipo: " ++ tipo m

-- Función principal para cargar y mostrar mobiliario
cargarYMostrarMobiliario :: FilePath -> FilePath -> IO ()
cargarYMostrarMobiliario archivoCSV archivoBD = do
    -- Leer mobiliario existente desde la base de datos
    mobiliarioExistente <- leerMobiliario archivoBD
    
    -- Cargar mobiliario desde el archivo CSV
    nuevoMobiliario <- cargarMobiliarioDesdeCSV archivoCSV
    
    -- Filtrar y agregar solo ítems con códigos únicos
    let duplicados = filter (\m -> codigoExiste (codigo m) mobiliarioExistente) nuevoMobiliario
    let mobiliarioFiltrado = filter (\m -> not (codigoExiste (codigo m) mobiliarioExistente)) nuevoMobiliario
    let mobiliarioActualizado = mobiliarioExistente ++ mobiliarioFiltrado
    
    -- Informar sobre duplicados
    if not (null duplicados)
        then do
            putStrLn "+-------------------------------------+"
            putStrLn "Se encontraron los siguientes duplicados:"
            mapM_ (putStrLn . codigo) duplicados
        else
            putStrLn "+-------------------------------------+" >>
            putStrLn "|     No se encontraron duplicados.   |" >>
            putStrLn "+-------------------------------------+" 
    
    -- Guardar la nueva base de datos actualizada
    guardarMobiliario archivoBD mobiliarioActualizado
    
    -- Mostrar todos los ítems de mobiliario
    mostrarMobiliario mobiliarioActualizado
