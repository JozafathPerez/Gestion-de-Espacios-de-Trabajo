{-# LANGUAGE DeriveGeneric #-}

module Reservas (leerReservas, buscarReservaPorCodigo, crearReserva, agregarReserva, eliminarReserva, editarReserva, validarDatosEdicion, consultarSalasDisponibles, consultarEstadoSalasEnRango, Reserva) where

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

import SalaReuniones (salaPorCodigo, leerNumeroCodigo, mostrarSala, SalaReuniones(..), leerSalas)
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

-- Función para guardar reservas en un archivo JSON
guardarReservas :: FilePath -> [Reserva] -> IO ()
guardarReservas path reservas = B.writeFile path (encode reservas)

--Funcion para agregar reserva
agregarReserva :: FilePath -> Reserva -> IO ()
agregarReserva archivo reservaNueva = do
    -- Leer reservas existentes
    resultado <- leerReservas archivo
    case resultado of
        Left err -> putStrLn ("Error al leer las reservas: " ++ err)
        Right reservasExistentes -> do
            -- Agregar la nueva reserva a la lista
            let reservasActualizadas = reservaNueva : reservasExistentes
            -- Guardar la lista actualizada de reservas en el archivo
            guardarReservas archivo reservasActualizadas
            putStrLn "Reserva guardada correctamente."

-- Funcion para imprimir una reserva
mostrarReserva :: Reserva -> IO ()
mostrarReserva reserva = do
    putStrLn "+-------------------------------------+"
    putStrLn "|       Información de Reserva        |"
    putStrLn "+-------------------------------------+"
    putStrLn $ "Código de reserva: " ++ codigoReserva reserva
    putStrLn $ "Código de sala: " ++ codigoSalaR reserva
    putStrLn $ "Código de usuario: " ++ codigoUsuario reserva
    putStrLn $ "Fecha de reserva: " ++ show (fecha reserva)
    putStrLn $ "Cantidad de personas: " ++ show (cantPersonas reserva)

-- Función para buscar una reserva por su código
buscarReservaPorCodigo :: FilePath -> String -> IO ()
buscarReservaPorCodigo archivoReservas codigoBuscado = do
    -- Leer las reservas desde el archivo
    resultado <- leerReservas archivoReservas
    case resultado of
        Left err -> putStrLn ("Error al leer las reservas: " ++ err)
        Right reservas -> do
            -- Buscar la reserva con el código especificado
            let reservaEncontrada = find (\r -> codigoReserva r == codigoBuscado) reservas
            case reservaEncontrada of
                Just reserva -> mostrarReserva reserva  
                Nothing -> putStrLn "No se encontró ninguna reserva con ese código."  

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
pedirCodigoUsuario :: IO String
pedirCodigoUsuario = do
    usuarios <- leerUsuarios "usuarios.json"
    case usuarios of
        Left err -> do
            putStrLn ("Error al leer el archivo de usuarios: " ++ err)
            return ""
        Right usuariosList -> do
            putStr "Ingrese el código del usuario: "
            hFlush stdout
            codigo <- getLine
            if isNothing (validarUsuario codigo usuariosList)
                then do
                    putStrLn "Usuario no encontrado. Por favor, ingrese un código válido."
                    pedirCodigoUsuario  -- Volver a pedir el código si no se encuentra el usuario
                else return codigo

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

-- Crear reserva
crearReserva :: [Reserva] -> IO Reserva
crearReserva reservasExistentes = do
    -- Pedir y validar el código de la sala
    sala <- pedirCodigoSala "salas.json"
    let codigoS = codigoSala sala

    -- Pedir y validar el código del usuario
    codigoU <- pedirCodigoUsuario

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

-- Función para eliminar una reserva por su código
eliminarReserva :: FilePath -> String -> IO ()
eliminarReserva archivoReservas codigoBuscado = do
    -- Leer las reservas desde el archivo
    resultado <- leerReservas archivoReservas
    case resultado of
        Left err -> putStrLn ("Error al leer las reservas: " ++ err)
        Right reservas -> do
            -- Buscar la reserva con el código especificado
            let reservasFiltradas = filter (\r -> codigoReserva r /= codigoBuscado) reservas
            if length reservasFiltradas < length reservas
                then do
                    -- Sobrescribir el archivo con las reservas filtradas
                    guardarReservas archivoReservas reservasFiltradas
                    putStrLn "Reserva eliminada con éxito."
                else putStrLn "No se encontró ninguna reserva con ese código."


-- Función para editar una reserva por su código
editarReserva :: FilePath -> String -> IO ()
editarReserva archivoReservas codigoBuscado = do
    -- Leer las reservas desde el archivo
    resultado <- leerReservas archivoReservas
    case resultado of
        Left err -> putStrLn ("Error al leer las reservas: " ++ err)
        Right reservas -> do
            -- Buscar la reserva con el código especificado
            let reservaEncontrada = find (\r -> codigoReserva r == codigoBuscado) reservas
            case reservaEncontrada of
                Just reserva -> do
                    putStrLn "Reserva encontrada. Ingrese los nuevos datos:"
                    
                    -- Pedir los nuevos valores para la reserva
                    nuevaReserva <- actualizarDatosReserva reserva
                    
                    -- Validar los nuevos datos de la reserva
                    let nuevoCodigoSala = codigoSalaR nuevaReserva
                    let nuevaFecha = fecha nuevaReserva
                    let nuevaCantidadPersonas = cantPersonas nuevaReserva
                    
                    -- Realizar las validaciones antes de proceder
                    validacionExitosa <- validarDatosEdicion nuevoCodigoSala nuevaFecha nuevaCantidadPersonas (fecha reserva)
                    if not validacionExitosa
                        then putStrLn "No se puede proceder con la edición debido a errores en la validación."
                        else do
                            -- Actualizar la lista de reservas con los nuevos datos
                            let reservasActualizadas = map (\r -> if codigoReserva r == codigoBuscado then nuevaReserva else r) reservas
                            -- Guardar las reservas actualizadas en el archivo
                            guardarReservas archivoReservas reservasActualizadas
                            putStrLn "Reserva actualizada con éxito."
                Nothing -> putStrLn "No se encontró ninguna reserva con ese código."


-- Función para actualizar los datos de la reserva
actualizarDatosReserva :: Reserva -> IO Reserva
actualizarDatosReserva reserva = do
    -- Pedir los nuevos valores para cada campo
    putStrLn "Ingrese el nuevo código de la sala (o deje en blanco para mantener el actual): "
    hFlush stdout
    nuevoCodigoSala <- getLine
    -- Pedri la nueva fecha
    putStrLn "Ingrese la nueva fecha de la reserva (dd/mm/yyyy) (o deje en blanco para mantener la actual): "
    hFlush stdout
    nuevaFechaStr <- getLine

    nuevaFecha <- if null nuevaFechaStr
        then return (fecha reserva)
        else pedirFechaDesdeCadena nuevaFechaStr

    putStrLn "Ingrese la nueva cantidad de personas (o deje en blanco para mantener la actual): "
    hFlush stdout
    nuevaCantidadPersonasStr <- getLine
    let nuevaCantidadPersonas = if null nuevaCantidadPersonasStr
                                then cantPersonas reserva
                                else read nuevaCantidadPersonasStr

    -- Crear la nueva reserva con los datos actualizados o los datos originales
    return Reserva {
        codigoReserva = codigoReserva reserva, -- Se mantiene el mismo código de reserva
        codigoSalaR = if null nuevoCodigoSala then codigoSalaR reserva else nuevoCodigoSala,
        codigoUsuario = codigoUsuario reserva, -- Se mantiene el mismo usuario
        fecha = nuevaFecha,
        cantPersonas = nuevaCantidadPersonas
    }

-- Función para validar si los datos de edición son correctos
validarDatosEdicion :: String -> Day -> Int -> Day -> IO Bool
validarDatosEdicion codiSala nuevaFecha cantPersonas fechaOriginal = do
    -- Leer reservas
    reservas <- leerReservas "reservas.json"
    -- Verificar si se pudo leer el archivo
    case reservas of
        Left err -> do
            putStrLn ("Error al leer las reservas: " ++ err)
            return False  -- Manejo de error
        -- Si se leyó correctamente
        Right reservasExistentes -> do
            -- Verificar si la sala existe
            sala <- salaPorCodigo "salas.json" codiSala
            case sala of
                Just salaValida -> do
                    -- Solo validar la fecha si es diferente a la original
                    if nuevaFecha /= fechaOriginal && existeReserva reservasExistentes codiSala nuevaFecha
                        then do
                            putStrLn "La fecha no está disponible para la sala seleccionada. Por favor, elija otra fecha."
                            return False
                        else do
                            if cantPersonas > capacidad salaValida
                                then do
                                    putStrLn "La cantidad de personas no puede exceder la capacidad de la sala."
                                    return False
                                else return True
                Nothing -> do
                    putStrLn "Sala no encontrada. Por favor, ingrese un código válido."
                    return False


-- Variación de la función para pedir una fecha, que recibe una cadena en lugar de leerla del usuario
pedirFechaDesdeCadena :: String -> IO Day
pedirFechaDesdeCadena fechaStr = do
    let formato = "%d/%m/%Y"
    case parseTimeM True defaultTimeLocale formato fechaStr of
        Just fechaValida -> return fechaValida
        Nothing -> do
            putStrLn "Fecha inválida. Por favor, ingrese una fecha válida."
            nuevaFechaStr <- getLine
            pedirFechaDesdeCadena nuevaFechaStr

-- Función para consultar las salas disponibles para una fecha
consultarSalasDisponibles :: IO ()
consultarSalasDisponibles = do
    -- Pedir la fecha a consultar
    fechaConsulta <- pedirFechaConsulta "Ingrese la fecha a consultar (dd/mm/yyyy): "

    -- Leer las salas desde el archivo
    resultadoSalas <- leerSalas "salas.json"
    case resultadoSalas of
        Left err -> putStrLn ("Error al leer las salas: " ++ err)
        Right salas -> do
            -- Leer las reservas desde el archivo
            resultadoReservas <- leerReservas "reservas.json"
            case resultadoReservas of
                Left err -> putStrLn ("Error al leer las reservas: " ++ err)
                Right reservas -> do
                    -- Filtrar las salas que están disponibles (no reservadas para esa fecha)
                    let salasDisponibles = filtrarSalasDisponibles salas reservas fechaConsulta

                    -- Mostrar las salas disponibles
                    if null salasDisponibles
                        then putStrLn "No hay salas disponibles para la fecha especificada."
                        else do
                            putStrLn "Salas disponibles para la fecha solicitada:"
                            mapM_ mostrarSala salasDisponibles

-- Filtrar las salas que no están reservadas para una fecha dada
filtrarSalasDisponibles :: [SalaReuniones] -> [Reserva] -> Day -> [SalaReuniones]
filtrarSalasDisponibles salas reservas fechaConsulta = 
    filter (\sala -> not (existeReserva reservas (codigoSala sala) fechaConsulta)) salas

-- Función para pedir una fecha desde la entrada del usuario
pedirFechaConsulta :: String -> IO Day
pedirFechaConsulta mensaje = do
    putStr mensaje
    hFlush stdout
    fechaStr <- getLine
    let formato = "%d/%m/%Y"
    case parseTimeM True defaultTimeLocale formato fechaStr of
        Just fechaValida -> return fechaValida
        Nothing -> do
            putStrLn "Fecha inválida. Por favor, ingrese una fecha válida."
            pedirFechaConsulta mensaje

-- Función para pedir el rango de fechas
pedirRangoFechas :: IO (Day, Day)
pedirRangoFechas = do
    fechaInicio <- pedirFechaConsulta "Ingrese la fecha inicial (dd/mm/yyyy): "
    fechaFin <- pedirFechaConsulta "Ingrese la fecha final (dd/mm/yyyy): "
    return (fechaInicio, fechaFin)

-- Generar una lista de fechas entre la fecha de inicio y la fecha de fin
generarFechasEnRango :: Day -> Day -> [Day]
generarFechasEnRango fechaInicio fechaFin = 
    [fechaInicio .. fechaFin]

-- Función para consultar el estado de las salas en un rango de fechas
consultarEstadoSalasEnRango :: IO ()
consultarEstadoSalasEnRango = do
    -- Pedir el rango de fechas
    (fechaInicio, fechaFin) <- pedirRangoFechas

    -- Generar la lista de fechas en el rango
    let fechasRango = generarFechasEnRango fechaInicio fechaFin

    -- Leer las salas desde el archivo
    resultadoSalas <- leerSalas "salas.json"
    case resultadoSalas of
        Left err -> putStrLn ("Error al leer las salas: " ++ err)
        Right salas -> do
            -- Leer las reservas desde el archivo
            resultadoReservas <- leerReservas "reservas.json"
            case resultadoReservas of
                Left err -> putStrLn ("Error al leer las reservas: " ++ err)
                Right reservas -> do
                    -- Mostrar el estado de las salas para cada fecha en el rango
                    mapM_ (mostrarEstadoSalasPorFecha reservas salas) fechasRango

-- Mostrar el estado de las salas para una fecha
mostrarEstadoSalasPorFecha :: [Reserva] -> [SalaReuniones] -> Day -> IO ()
mostrarEstadoSalasPorFecha reservas salas fecha = do
    putStrLn $ "\nEstado de las salas para la fecha: " ++ show fecha
    putStrLn "+--------------------------+"
    mapM_ (mostrarEstadoSala fecha reservas) salas

-- Mostrar si una sala está reservada o disponible para una fecha específica
mostrarEstadoSala :: Day -> [Reserva] -> SalaReuniones -> IO ()
mostrarEstadoSala fecha reservas sala = do
    let estaReservada = existeReserva reservas (codigoSala sala) fecha
    putStrLn $ "Sala: " ++ codigoSala sala ++ " - "  ++  if estaReservada then "Reservada" else "Disponible"
