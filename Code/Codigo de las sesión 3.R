################################################################################
################################ Sesion 3 ######################################
################################################################################

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

# Se crea un data frame llamado 'datos_encuesta' con 4 filas y 5 columnas
datos_encuesta <- data.frame(
  id = 1:4,  # Columna 'id' con números del 1 al 4 (identificadores únicos para cada fila)
  fecha = seq(as.Date("2023-01-01"), by = "month", length.out = 4), # Columna 'fecha' con una secuencia de fechas mensuales a partir del 1 de enero de 2023
  genero = c("M", "F", "F", "M"), # Columna 'sexo' con los valores "M" (masculino) y "F" (femenino)
  edad = c(34, 45, 29, 62), # Columna 'edad' con edades de las personas encuestadas
  fumador = c(1, 0, 1, 0), # Columna 'fumador' con 1 = sí fuma, 0 = no fuma
  percepcion = c("Alta", "Baja", "Media", "Alta") # percepcion
)

# Se muestra el contenido del data frame 'datos_encuesta' en consola
datos_encuesta

#-------------------------------------------------------------------------------

id <-  1:4  # Columna 'id' con números del 1 al 4 (identificadores únicos para cada fila)
fecha <-  seq(as.Date("2023-01-01"), by = "month", length.out = 4) # Columna 'fecha' con una secuencia de fechas mensuales a partir del 1 de enero de 2023
genero <- c("M", "F", "F", "M") # Columna 'sexo' con los valores "M" (masculino) y "F" (femenino)
edad <- c(34, 45, 29, 62) # Columna 'edad' con edades de las personas encuestadas
fumador <- c(1, 0, 1, 0) # Columna 'fumador' con 1 = sí fuma, 0 = no fuma
percepcion <- c("Alta", "Baja", "Media", "Alta") # percepcion

# Se crea un data frame llamado 'datos_encuesta' con 4 filas y 5 columnas
datos_encuesta <- data.frame(id, fecha, genero, fumador, percepcion)

# Se muestra el contenido del data frame 'datos_encuesta' en consola
datos_encuesta

# Se explora la estructura de los datos
str(datos_encuesta)

#-------------------------------------------------------------------------------

datos_encuesta$genero <- factor(datos_encuesta$genero,
                                levels = c("M", "F"),
                                labels = c("Masculino", "Femenino"))
str(datos_encuesta)

#-------------------------------------------------------------------------------

datos_encuesta$percepcion <- factor(datos_encuesta$percepcion,
                                    levels = c("Baja", "Media", "Alta"),
                                    ordered = TRUE)
str(datos_encuesta)

#-------------------------------------------------------------------------------

datos_encuesta$fumador <- factor(datos_encuesta$fumador,
                                 levels = c(0, 1), labels = c("No", "Sí"))
str(datos_encuesta)

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

# Cargar datos desde un archivo CSV ubicado en el mismo directorio de trabajo
datos <- read.csv("BD-EEVV-Defuncionesnofetales-2023.csv")

# Determinar las dimensiones de data.frame
dim(datos)        # Dimensiones del data frame (filas y columnas)

# Muestra la estructura del objeto (tipos de variables, ejemplos de valores)
str(datos)        

# Imprimir todo el contenido del data frame no es recomendado si hay muchas filas
head(datos)       # Primeras 6 filas
tail(datos)       # Últimas 6 filas

# Lista los nombres de las variables (columnas)
names(datos)      # Lista los nombres de las variables (columnas)
colnames(datos)   # Lista los nombres de las variables (columnas)

#-------------------------------------------------------------------------------

library(rio)

# Cargar un archivo .csv
datos <- import("BD-EEVV-Defuncionesnofetales-2023.csv")

# Ver el contenido
head(datos)

# Seleccionar variables
datos_analisis <- datos %>% select(COD_DPTO,
                                   COD_MUNIC,
                                   A_DEFUN,
                                   SEXO,
                                   GRU_ED2,
                                   NIVEL_EDU,
                                   CAUSA_667,
                                   SIMUERTEPO,
                                   EMB_FAL)