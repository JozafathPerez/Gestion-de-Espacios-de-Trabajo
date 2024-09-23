-- Mobiliario.hs
module Mobiliario (cargarYMostrarMobiliario) where

import System.IO (hFlush, stdout)
import Data.List.Split (splitOn)
import Data.Aeson (eitherDecode, encode)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import Data.List (find)
import Data.Maybe (isJust)

data Mobiliario = Mobiliario {
    codigo :: String,
    nombre :: String,
    descripcion :: String,
    tipo :: String  -- Puede ser consumible, electrónico o menaje
} deriving (Show, Eq)

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
    return (map parseLineaMobiliario lineas)

parseLineaMobiliario :: String -> Mobiliario
parseLineaMobiliario linea =
    let campos = splitOn "," linea
    in Mobiliario (campos !! 0) (campos !! 1) (campos !! 2) (campos !! 3)

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
    let mobiliarioFiltrado = filter (\m -> not (codigoExiste (codigo m) mobiliarioExistente)) nuevoMobiliario
    let mobiliarioActualizado = mobiliarioExistente ++ mobiliarioFiltrado
    
    -- Guardar la nueva base de datos actualizada
    guardarMobiliario archivoBD mobiliarioActualizado
    
    -- Mostrar todos los ítems de mobiliario
    mostrarMobiliario mobiliarioActualizado
