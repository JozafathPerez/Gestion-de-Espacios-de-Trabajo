import System.IO (hFlush, stdout)
import Utils (leerUsuarios, validarUsuario, Usuario(..))
import Mobiliario (cargarYMostrarMobiliario)
import Data.Maybe (isNothing, fromJust)

showOperativas :: Usuario -> IO ()
showOperativas usuario = do
    putStrLn "+-------------------------------------+"
    putStrLn ("Bienvenido, " ++ nombreCompleto usuario ++ " (" ++ puesto usuario ++ ")")
    putStrLn "+-------------------------------------+"
    putStrLn "+-------------------------------------+"
    putStrLn "|        OPCIONES OPERATIVAS          |"
    putStrLn "+-------------------------------------+"
    putStrLn "| 1 | Crear y Mostrar Mobiliario      |"
    putStrLn "| 2 | Cargar y Mostrar Salas          |"
    putStrLn "| 3 | Informe de Reservas             |"
    putStrLn "| 4 | Volver al Menú Principal        |"
    putStrLn "+-------------------------------------+"
    putStr "Seleccione una opción: "
    hFlush stdout

handleOperativas :: Usuario -> String -> IO ()
handleOperativas usuario option = case option of
    "1" -> do
        putStrLn "Opción: Crear y Mostrar Mobiliario"
        putStrLn "Ingrese la ruta del archivo CSV de mobiliario: "
        hFlush stdout
        archivoCSV <- getLine
        let archivoBD = "mobiliario.json"  -- Archivo JSON para almacenar la base de datos de mobiliario
        cargarYMostrarMobiliario archivoCSV archivoBD
        menuOperativas usuario -- Volver al menú operativas
    "2" -> do
        putStrLn "Opción: Cargar y Mostrar Salas"
        -- Lógica para cargar y mostrar salas
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
        menuGenerales 
    "2" -> do
        putStrLn "Opción: Consultar Reserva"
        menuGenerales
    "3" -> do
        putStrLn "Opción: Cancelar/Modificar Reserva"
        menuGenerales
    "4" -> do
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
    putStrLn "|      GESTIÓN DE SALAS         |"
    putStrLn "+-------------------------------+"
    putStrLn "| 1 | Opciones Operativas       |"
    putStrLn "| 2 | Opciones Generales        |"
    putStrLn "| 3 | Salir                     |"
    putStrLn "+-------------------------------+"
    putStr "Seleccione una opción: "
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
