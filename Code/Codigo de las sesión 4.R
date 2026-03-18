################################################################################
################################ Sesion 4 ######################################
################################################################################

# ------------------------------------------------------------------------------

library(rio)

# Cargar un archivo .csv
datos <- import("BD-EEVV-Defuncionesnofetales-2023.csv")

# Ver el contenido
head(datos)

# ------------------------------------------------------------------------------

library(tidyverse)

# Manipulación de datos dplyr:
# Seleccionar variables select()

datos_analisis <- datos %>% select(COD_DPTO,
                                   COD_MUNIC,
                                   A_DEFUN,
                                   SEXO,
                                   GRU_ED2,
                                   NIVEL_EDU,
                                   CAUSA_667,
                                   SIMUERTEPO,
                                   EMB_FAL)
str(datos_analisis)

# ------------------------------------------------------------------------------

library(skimr)

# skim() proporcionar rápidamente una descripción general amplia de un marco de datos.
skim(datos_analisis)

# ------------------------------------------------------------------------------

library(naniar)

# naniar es una libreria que proporciona métodos claros y ordenados para resumir, visualizar y manipular datos faltantes.
datos_analisis %>% miss_var_summary()
datos_analisis %>% gg_miss_var(show_pct = TRUE)

# ------------------------------------------------------------------------------

# Eliminar variables select()
datos_analisis <- datos_analisis %>% select(-SIMUERTEPO, -EMB_FAL)
str(datos_analisis)

# ------------------------------------------------------------------------------

# Renombrar variables rename()
datos_analisis <- datos_analisis %>% rename(
  codigo_departamento = COD_DPTO,
  codigo_municipio    = COD_MUNIC,
  area_defuncion      = A_DEFUN,
  sexo                = SEXO,
  grupo_edad          = GRU_ED2,
  nivel_educativo     = NIVEL_EDU,
  causa_muerte_667    = CAUSA_667
)

str(datos_analisis)

# ------------------------------------------------------------------------------

# Poner etiquetas factor()

# Convertir area_defuncion en factor
datos_analisis$area_defuncion <- factor(datos_analisis$area_defuncion,
                                        levels = c(1, 2, 3, 9),
                                        labels = c("Cabecera municipal",
                                                   "Centro poblado (Inspección, corregimiento o caserío)",
                                                   "Rural disperso",
                                                   "Sin información"))
head(datos_analisis)

# Convertir sexo en factor
datos_analisis$sexo <- factor()
head(datos_analisis)

# Convertir grupo_edad en factor
datos_analisis$grupo_edad <- factor(datos_analisis$grupo_edad,
                                    levels = c(1, 2, 3, 4, 5, 6, 7),
                                    labels = c("Menor de 1 año",
                                               "De 1 a 4 aos",
                                               "De 5 a 14 aos",
                                               "De 15 a 44 aos",
                                               "De 45 a 64 aos",
                                               "De 65 y más aos",
                                               "Edad desconocida"), 
                                    ordered = TRUE)
head(datos_analisis)

# Convertir nivel_educativo en factor
datos_analisis$nivel_educativo <- factor(datos_analisis$nivel_educativo,
                                         levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 99),
                                         labels = c("Preescolar",
                                                    "Básica primaria",
                                                    "Básica secundaria",
                                                    "Media académica o clásica",
                                                    "Media técnica",
                                                    "Normalista",
                                                    "Técnica profesional",
                                                    "Tecnológica",
                                                    "Profesional",
                                                    "Especialización",
                                                    "Maestría",
                                                    "Doctorado",
                                                    "Ninguno",
                                                    "Sin información"), 
                                         ordered = TRUE)
head(datos_analisis)

# ------------------------------------------------------------------------------

# Revisar como quedaron las nuevas variables
str(datos_analisis)

# ------------------------------------------------------------------------------

# Remplazar texto gsub()
datos_analisis$grupo_edad <- gsub("aos", "años", datos_analisis$grupo_edad)
head(datos_analisis$grupo_edad)

# ------------------------------------------------------------------------------

# Filtrar datos filter()
datos_analisis_m <- datos_analisis %>% filter(sexo == "Masculino")
datos_analisis_f <- datos_analisis %>% filter(sexo == "Femenino")
head(datos_analisis_f)
head(datos_analisis_m)

# ------------------------------------------------------------------------------

# Selecciona las filas donde la variable sexo sea “Femenino” o “Masculino”.
datos_analisis_f_m <- datos_analisis %>% filter(sexo == "Femenino" | sexo == "Masculino")
head(datos_analisis_f_m)

# Selecciona las filas donde la variable sexo sea “Femenino” y la variable area_defuncion sea “Cabecera municipal”.
datos_analisis_f_cm <- datos_analisis %>% filter(sexo == "Femenino" & area_defuncion == "Cabecera municipal")
head(datos_analisis_f_cm)

# Selecciona las filas donde el sexo no sea “Femenino” y el area_defuncion sea “Cabecera municipal”.
datos_analisis_mi_cm <- datos_analisis %>% filter(sexo != "Femenino" & area_defuncion == "Cabecera municipal")
head(datos_analisis_mi_cm)

# ------------------------------------------------------------------------------

# Agrupar datos y resumirlos group_by() y summarise()
# Calcula el total de casos por grupo y el porcentaje respecto al total general usando.

datos_analisis %>%
  select(sexo) %>% 
  group_by(sexo) %>%
  summarise(total = n(), porcentaje = (n() / nrow(datos_analisis)) * 100)

# ------------------------------------------------------------------------------

# Agregar columnas mutate()
# Esto agrega una nueva columna llamada id con un número consecutivo para cada fila, empezando desde 1.
datos_analisis <- datos_analisis %>% 
  mutate(id = row_number()) %>% 
  relocate(id, .before = 1)

# ------------------------------------------------------------------------------

# Ordenar filas arrange()
# arrange() ordena las filas de un marco de datos según los valores de las columnas seleccionadas.

datos_ordenados <- datos_analisis %>%
  arrange(desc(codigo_municipio))

# ------------------------------------------------------------------------------

# Datos duplicados distinct()

# Creamos un data frame con filas duplicadas
df <- data.frame(
  nombre = c("Ana", "Luis", "Ana", "Pedro", "Luis"),
  edad   = c(23, 30, 23, 40, 30),
  ciudad = c("Bogotá", "Medellín", "Bogotá", "Cali", "Medellín")
)

# Ver los datos originales
df

# Eliminar duplicados considerando todas las columnas
df_unico <- df %>%
  distinct()

df_unico

# ------------------------------------------------------------------------------

# Condicionadores case_when()
datos_analisis <- datos_analisis %>%
  mutate(
    grupo_edad_cat = case_when(
      grupo_edad %in% c("Menor de 1 año", "De 1 a 4 años", "De 5 a 14 años") ~ "Menores de 15 años",
      grupo_edad %in% c("De 15 a 44 años", "De 45 a 64 años") ~ "Adultos 15-64",
      grupo_edad %in% c("De 65 y más años") ~ "Adultos mayores 65+",
      grupo_edad %in% c("Edad desconocida") ~ "Desconocido",
      TRUE ~ NA_character_   # por si aparece algo inesperado
    )
  )

head(datos_analisis)

# install.packages("fastDummies")
library(fastDummies)

# Simular una base de datos
set.seed(123)
n <- 10
estrato <- sample(0:5, n, replace = TRUE)
datos <- data.frame(id = 1:n, estrato = estrato)

# Crear dummies rápido
# dummy_cols() crea columnas binarias (0/1) para cada categoría
# remove_selected_columns = TRUE elimina la variable categórica original
# remove_first_dummy = FALSE mantiene todas las dummies (no elimina ninguna)
datos_dummies <- dummy_cols(datos, select_columns = "estrato", remove_first_dummy = FALSE, remove_selected_columns = TRUE)

datos_dummies

# Queremos confirmar que en cada fila solo haya un "1"

# apply(X, MARGIN, FUN):
# - X: el data frame o matriz
# - MARGIN = 1: aplicar función por filas (si fuera 2 sería por columnas)
# - FUN = sum: sumar los valores en cada fila

# Sumar valores por fila (solo columnas dummy)
row_sums <- apply(datos_dummies[ , -1], 1, sum)  # Quitamos la columna i

# Verificar que todas las filas sumen 1
all(row_sums == 1)

# Reconstruir la variable estrato
datos_recuperados <- datos_dummies %>%
  mutate(
    estrato = case_when(
      estrato_1 == 1 ~ 1,
      estrato_2 == 1 ~ 2,
      estrato_3 == 1 ~ 3,
      estrato_4 == 1 ~ 4,
      estrato_5 == 1 ~ 5
    )
  )
head(datos_recuperados)

# ------------------------------------------------------------------------------

# Tasa de Mortalidad

# Conteo de defunciones por sexo
conteo_defunciones <- datos_analisis %>%
  group_by(sexo) %>%
  summarise(defunciones = n())
conteo_defunciones

# Asignar población “inventada” para cada sexo (para el ejemplo educativo)
# 
# sample(20000000:30000000, n()) genera números aleatorios entre 20 y 30 millones (población ficticia).
# 
# mutate(poblacion = …) añade esta columna de población a la tabla

conteo_defunciones <- conteo_defunciones %>%
  mutate(poblacion = sample(20000000:30000000, n()))
conteo_defunciones

# Calcular tasa de mortalidad por 1000 habitantes
conteo_defunciones %>%
  mutate(tasa_mortalidad = (defunciones / poblacion) * 1000)


