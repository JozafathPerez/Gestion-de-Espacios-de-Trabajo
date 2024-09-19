import System.IO (hFlush, stdout)

showOperativas :: IO ()
showOperativas = do
    putStrLn "+-------------------------------------+"
    putStrLn "|        OPCIONES OPERATIVAS          |"
    putStrLn "+-------------------------------------+"
    putStrLn "| 1 | Crear y Mostrar Mobiliario      |"
    putStrLn "| 2 | Cargar y Mostrar Salas          |"
    putStrLn "| 3 | Informe de Reservas             |"
    putStrLn "| 4 | Volver al Menú Principal        |"
    putStrLn "+-------------------------------------+"
    putStr "Seleccione una opción:"
    hFlush stdout

-- Manejar Opciones Operativas
handleOperativas :: String -> IO ()
handleOperativas option = case option of
    "1" -> do
        putStrLn "Opción: Crear y Mostrar Mobiliario"
        -- Aquí va la lógica para crear y mostrar mobiliario
        menuOperativas
    "2" -> do
        putStrLn "Opción: Cargar y Mostrar Salas"
        -- Aquí va la lógica para cargar y mostrar salas
        menuOperativas
    "3" -> do
        putStrLn "Opción: Informe de Reservas"
        -- Aquí va la lógica para mostrar el informe de reservas
        menuOperativas
    "4" -> do
        putStrLn "Volviendo al Menú Principal..."
        mainMenu
    _ -> do
        putStrLn "+-------------------------------------+"
        putStrLn "| Opción no válida. Intente de nuevo. |"
        putStrLn "+-------------------------------------+"
        menuOperativas

-- Menú de Opciones Operativas
menuOperativas :: IO ()
menuOperativas = do
    showOperativas
    option <- getLine
    handleOperativas option

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
    putStr "Seleccione una opción:"
    hFlush stdout

-- Manejar Opciones Generales
handleGenerales :: String -> IO ()
handleGenerales option = case option of
    "1" -> do
        putStrLn "Opción: Gestión de Reserva"
        -- Aquí va la lógica para gestionar reservas
        menuGenerales 
    "2" -> do
        putStrLn "Opción: Consultar Reserva"
        -- Aquí va la lógica para consultar reservas
        menuGenerales
    "3" -> do
        putStrLn "Opción: Cancelar/Modificar Reserva"
        -- Aquí va la lógica para cancelar o modificar reservas
        menuGenerales
    "4" -> do
        putStrLn "Opción: Consulta Disponibilidad Sala"
        -- Aquí va la lógica para consultar disponibilidad
        menuGenerales
    "5" -> do
        putStrLn "Volviendo al Menú Principal..."
        mainMenu
    _ -> do
        putStrLn "+-------------------------------------+"
        putStrLn "| Opción no válida. Intente de nuevo. |"
        putStrLn "+-------------------------------------+"
        menuGenerales

-- Menú de Opciones Generales
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
    putStr "Seleccione una opción:"
    hFlush stdout

handleMenu :: String -> IO ()
handleMenu option = case option of
    "1" -> do
        putStrLn "Accediendo a Opciones Operativas..."
        menuOperativas
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