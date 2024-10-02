import System.IO (hFlush, stdout)
import Usuarios (leerUsuarios, validarUsuario, Usuario(..))
import Mobiliario (cargarYMostrarMobiliario)
import SalaReuniones (crearYMostrarSala, mostrarSalaPorCodigo)
import Reservas (leerReservas, crearReserva, agregarReserva, buscarReservaPorCodigo, eliminarReserva, editarReserva, validarDatosEdicion)
import Data.Maybe (isNothing, fromJust)

import Data.Time (Day, parseTimeM, defaultTimeLocale, fromGregorian)

showOperativas :: Usuario -> IO ()
showOperativas usuario = do
    putStrLn "+-------------------------------------+"
    putStrLn ("Bienvenido, " ++ nombreCompleto usuario ++ " (" ++ puesto usuario ++ ")")
    putStrLn "+-------------------------------------+"
    putStrLn "+-------------------------------------+"
    putStrLn "|        OPCIONES OPERATIVAS          |"
    putStrLn "+-------------------------------------+"
    putStrLn "| 1 | Cargar y Mostrar Mobiliario     |"
    putStrLn "| 2 | Crear y Mostrar Salas           |"
    putStrLn "| 3 | Informe de Reservas             |"
    putStrLn "| 4 | Volver al Menú Principal        |"
    putStrLn "+-------------------------------------+"
    putStr "Seleccione una opción: "
    hFlush stdout

menuSalasR :: FilePath -> FilePath -> IO ()
menuSalasR archivoMobiliario archivoSalas = do
    putStrLn "\n+--------------------------------------+"
    putStrLn "|      Menú de Salas de Reunión          |"
    putStrLn "+----------------------------------------+"
    putStrLn "| 1 | Crear una nueva sala de reunión    |"
    putStrLn "| 2 | Ver una sala de reunión por código |"
    putStrLn "| 0 | Salir al menu operativas.          |"
    putStrLn "+----------------------------------------+"
    putStr "Seleccione una opción: "
    hFlush stdout
    opcion <- getLine
    case opcion of
        "1" -> crearYMostrarSala archivoMobiliario archivoSalas >> menuSalasR archivoMobiliario archivoSalas
        "2" -> do
            putStr "Ingrese el código de la sala: "
            hFlush stdout
            codigo <- getLine
            mostrarSalaPorCodigo archivoSalas codigo
            menuSalasR archivoMobiliario archivoSalas
        "0" -> putStrLn "Saliendo del sistema."
        _   -> putStrLn "Opción no válida." >> menuSalasR archivoMobiliario archivoSalas


handleOperativas :: Usuario -> String -> IO ()
handleOperativas usuario option = case option of
    "1" -> do
        putStrLn "Ingrese la ruta del archivo CSV de mobiliario: "
        hFlush stdout
        archivoCSV <- getLine
        let archivoBD = "mobiliario.json"  
        cargarYMostrarMobiliario archivoCSV archivoBD
        menuOperativas usuario -- Volver al menú operativas
    "2" -> do
        let archivoBDMobiliario = "mobiliario.json"
        let archivoBDSalas = "salas.json"
        menuSalasR archivoBDMobiliario archivoBDSalas
        menuOperativas usuario
    "3" -> do
        putStrLn "Opción: Informe de Reservas"
        -- Lógica para mostrar informe de reservas
        menuOperativas usuario
    "4" -> do
        putStrLn "Volviendo al Menú Principal..."
        mainMenu
    _ -> do
        putStrLn "+-------------------------------------+"
        putStrLn "| Opción no válida. Intente de nuevo. |"
        putStrLn "+-------------------------------------+"
        menuOperativas usuario

solicitarIDUsuario :: IO String
solicitarIDUsuario = do
    putStr "Ingrese su ID de usuario (cédula): "
    hFlush stdout
    getLine

validarUsuarioMenu :: IO (Maybe Usuario)
validarUsuarioMenu = do
    usuarios <- leerUsuarios "usuarios.json"
    case usuarios of
        Left err -> do
            putStrLn ("Error al leer el archivo de usuarios: " ++ err)
            return Nothing
        Right usuariosList -> do
            idUsuario <- solicitarIDUsuario
            let usuarioValido = validarUsuario idUsuario usuariosList
            if isNothing usuarioValido
                then do
                    putStrLn "ID de usuario no válido. Intente nuevamente."
                    validarUsuarioMenu
                else do
                    let usuario = fromJust usuarioValido
                    return (Just usuario)

menuOperativas :: Usuario -> IO ()
menuOperativas usuario = do
    showOperativas usuario
    option <- getLine
    handleOperativas usuario option

subMenuGenerales  :: FilePath -> IO ()
subMenuGenerales archivoReservas = do
    putStrLn "\n+--------------------------------------+"
    putStrLn "|     Menú de Edición de Reservas        |"
    putStrLn "+----------------------------------------+"
    putStrLn "| 1 | Cancelación de reserva.            |"
    putStrLn "| 2 | Modificación de reserva.           |"
    putStrLn "| 0 | Salir al menu Generales.           |"
    putStrLn "+----------------------------------------+"
    putStr "Seleccione una opción: "
    hFlush stdout
    opcion <- getLine
    case opcion of
        "1" -> do
            putStr "Ingrese el código de la reserva a cancelar: "
            hFlush stdout
            codigo <- getLine
            eliminarReserva archivoReservas codigo
            subMenuGenerales archivoReservas

        "2" -> do
            putStrLn "Ingrese el código de la reserva a editar: "
            hFlush stdout
            codigo <- getLine
            editarReserva "reservas.json" codigo
            subMenuGenerales archivoReservas
        "0" -> putStrLn "Saliendo del sistema."
        _   -> putStrLn "Opción no válida." >> subMenuGenerales archivoReservas

showGenerales :: IO ()
showGenerales = do
    putStrLn "+-----------------------------------+"
    putStrLn "|        OPCIONES GENERALES         |"
    putStrLn "+-----------------------------------+"
    putStrLn "| 1 | Gestión de Reserva            |"
    putStrLn "| 2 | Consultar Reserva             |"
    putStrLn "| 3 | Cancelar/Modificar Reserva    |"
    putStrLn "| 4 | Consulta Disponibilidad Sala  |"
    putStrLn "| 5 | Volver al Menú Principal      |"
    putStrLn "+-----------------------------------+"
    putStr "Seleccione una opción: "
    hFlush stdout

handleGenerales :: String -> IO ()
handleGenerales option = case option of
    "1" -> do
        putStrLn "Opción: Gestión de Reserva"
        -- Leer las reservas existentes
        reservasResult <- leerReservas "reservas.json"
        
        case reservasResult of
            Left err -> do
                putStrLn ("Error al leer las reservas: " ++ err)
                return ()  -- Manejo de error
            Right reservasExistentes -> do
                -- Crear una nueva reserva
                nuevaReserva <- crearReserva reservasExistentes
                putStrLn ("Reserva creada: " ++ show nuevaReserva)
                -- Agregar la nueva reserva a la lista
                agregarReserva "reservas.json" nuevaReserva

        menuGenerales 
    "2" -> do
        putStrLn "Opción: Consultar Reserva"
        -- Leer las reservas existentes
        putStr "Ingrese el codigo de reserva:"
        hFlush stdout
        codigo <- getLine
        buscarReservaPorCodigo "reservas.json" codigo

        menuGenerales
    "3" -> do
        subMenuGenerales "reservas.json"
        menuGenerales
    "4" -> do
        -- Probar con un código de sala existente, una fecha y cantidad de personas válida
        putStrLn "Opción: Consulta Disponibilidad Sala"
        
        menuGenerales
    "5" -> do
        putStrLn "Volviendo al Menú Principal..."
        mainMenu
    _ -> do
        putStrLn "+-------------------------------------+"
        putStrLn "| Opción no válida. Intente de nuevo. |"
        putStrLn "+-------------------------------------+"
        menuGenerales

menuGenerales :: IO ()
menuGenerales = do
    showGenerales
    option <- getLine
    handleGenerales option

showMenu :: IO ()
showMenu = do
    putStrLn "+-------------------------------+"
    putStrLn "|      GESTIÓN DE SALAS    cd     |"
    putStrLn "+-------------------------------+"
    putStrLn "| 1 | Opciones Operativas       |"
    putStrLn "| 2 | Opciones Generales        |"
    putStrLn "| 3 | Salir                     |"
    putStrLn "+-------------------------------+"
    putStr "Seleccione una opción:  "
    hFlush stdout

handleMenu :: String -> IO ()
handleMenu option = case option of
    "1" -> do
        usuarioValido <- validarUsuarioMenu
        case usuarioValido of
            Just usuario -> menuOperativas usuario
            Nothing -> do
                putStrLn "Error al validar el usuario. No se puede continuar."
                mainMenu
    "2" -> do
        putStrLn "Accediendo a Opciones Generales..."
        menuGenerales
    "3" -> do
        putStrLn "+---------------------------+"
        putStrLn "| Saliendo del programa...  |"
        putStrLn "+---------------------------+"
    _ -> do
        putStrLn "+-------------------------------------+"
        putStrLn "| Opción no válida. Intente de nuevo. |"
        putStrLn "+-------------------------------------+"
        mainMenu

mainMenu :: IO ()
mainMenu = do
    showMenu
    option <- getLine
    handleMenu option

main :: IO ()
main = mainMenu
