# Nombre del ejecutable
EXEC = main

# Nombre del archivo fuente
SRC = main.hs 

# Regla por defecto: compilar y ejecutar
all: build run

# Regla para compilar el archivo fuente
build:
    ghc -o $(EXEC) $(SRC)

# Regla para ejecutar el programa compilado
run:
    $(EXEC).exe

# Regla para limpiar los archivos generados por la compilación
clean:
    del $(EXEC).exe $(EXEC).hi $(EXEC).o