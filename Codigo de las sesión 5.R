################################################################################
######################## Taller de Estadística Básica en R #####################
####################   Análisis de datos COVID-19 INS (Ejemplo) ################
################################################################################

#------------------------------------------------------------------------------
# 1. Librerías
#------------------------------------------------------------------------------
library(rio)        # Para importar/exportar datos en múltiples formatos (Excel, CSV...)
library(tidyverse)  # Conjunto de paquetes para manipulación y visualización (dplyr, ggplot2...)
library(gtsummary)  # Crear tablas de resumen descriptivo
library(skimr)      # Resumen rápido de la base (estructura y estadísticas)
library(naniar)     # Análisis y visualización de datos faltantes
library(DescTools)  # Funciones estadísticas adicionales (ej: Moda)
library(scales)     # Escalas y formateo en gráficos (ej. porcentajes)
library(sf)         # Manejo de datos espaciales y shapefiles (mapas)

#------------------------------------------------------------------------------
# 2. Lectura de los datos
#------------------------------------------------------------------------------
datos <- import("Data_COVID_INS_5302020.xlsx") # Carga la base de datos del INS
View(datos)  # (opcional) inspeccionar los datos en pestaña estilo Excel

#------------------------------------------------------------------------------
# 3. Limpieza y transformación de variables
#------------------------------------------------------------------------------

# Argumentos / columnas:
# - Fecha Not: Fecha de notificación del caso.
# - Fecha de inicio de síntomas: Fecha en que comenzaron los síntomas.
# - Fecha de diagnóstico: Fecha en que se confirmó el diagnóstico.
# - Fecha de muerte: Fecha de fallecimiento (si aplica).
# - Fecha recuperado: Fecha en que el paciente fue declarado recuperado.

# Todas las fechas están en formato "año/mes/día" ("%Y/%m/%d").

# Convertir fechas de texto a formato fecha (Date)
datos$`Fecha Not` <- as.Date(datos$`Fecha Not`, format = "%Y/%m/%d")
datos$`Fecha de inicio de síntomas` <- as.Date(datos$`Fecha de inicio de síntomas`, format = "%Y/%m/%d")
datos$`Fecha de diagnóstico` <- as.Date(datos$`Fecha de diagnóstico`, format = "%Y/%m/%d")
datos$`Fecha de muerte` <- as.Date(datos$`Fecha de muerte`, format = "%Y/%m/%d")
datos$`Fecha recuperado` <- as.Date(datos$`Fecha recuperado`, format = "%Y/%m/%d")

# Convertir variables categóricas a factor con etiquetas
datos$Sexo <- factor(datos$Sexo, levels = c("F", "M"), labels = c("Femenino", "Masculino"))
datos$Departamento <- as.factor(datos$Departamento)
datos$Ciudad <- as.factor(datos$Ciudad)
datos$Tipo <- as.factor(datos$Tipo)
datos$Ubicación <- as.factor(datos$Ubicación)
datos$Estado <- as.factor(datos$Estado)
datos$`Pais de procedencia` <- as.factor(datos$`Pais de procedencia`)
datos$Recuperacion <- as.factor(datos$Recuperacion)

# Renombrar columnas: reemplaza espacios por "_"
names(datos) <- gsub(" ", "_", names(datos))

#------------------------------------------------------------------------------
# 4. Exploración inicial
#------------------------------------------------------------------------------
skim(datos)                       # Resumen general (conteo, medias, etc.)
datos %>% gg_miss_var(show_pct=T) # Gráfico: % de datos faltantes por variable
datos %>% miss_var_summary()      # Tabla con conteo de datos faltantes

################################################################################
######################## Visualización de variables ############################
################################################################################

#------------------------------------------------------------------------------
# 5. Histograma de edad
#------------------------------------------------------------------------------

# Este script genera un histograma de la variable 'Edad' en el data frame 'datos'.
# Se utiliza ggplot2, una de las librerías más populares en R para visualización de datos.

# Argumentos / partes del gráfico:
# - datos: el data frame que contiene la variable 'Edad'.
# - aes(x = Edad): se especifica que la variable 'Edad' estará en el eje X.
# - geom_histogram(): crea el histograma con 30 barras (bins), relleno azul y bordes blancos.
# - labs(): añade título y etiquetas a los ejes.
# - theme_minimal(): aplica un tema limpio con fondo claro.

ggplot(datos, aes(x = Edad)) +
  geom_histogram(fill = "#F87C63", color = "white") +
  labs(title = "Histograma de la edad de los casos",
       x = "Edad (años)", y = "Frecuencia") +
  theme_bw()

#------------------------------------------------------------------------------
# 6. Distribución por sexo
#------------------------------------------------------------------------------

# Este script genera un gráfico de barras que muestra la distribución de los casos por sexo
# usando la librería ggplot2. Se agregan colores personalizados y etiquetas de conteo encima de cada barra.

# Argumentos / componentes del gráfico:
# - datos: data frame que contiene la variable 'Sexo'.
# - aes(x = Sexo, fill = Sexo): se usa la variable 'Sexo' para el eje X y para asignar colores.
# - geom_bar(): crea las barras según la frecuencia de cada categoría en 'Sexo'.
# - geom_text(): añade etiquetas con el número de casos sobre cada barra.
# - scale_fill_manual(): define manualmente los colores para cada valor de 'Sexo'.
# - labs(): añade título y etiquetas a los ejes.
# - theme_minimal(): aplica un estilo visual simple y limpio.

ggplot(datos, aes(x = Sexo, fill = Sexo)) +
  geom_bar(color = "white", width = 0.4) +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("Femenino" = "#A684FF", "Masculino" = "#3498DB")) +
  labs(title = "Distribución de casos por sexo", x = "Sexo", y = "Frecuencia") +
  theme_minimal()

################################################################################
####################### Estadísticos descriptivos ##############################
################################################################################

#------------------------------------------------------------------------------
# 8. Medidas de tendencia central
#------------------------------------------------------------------------------

# Se utiliza 'na.rm = TRUE' para ignorar los valores NA (faltantes) en los cálculos.

mean(datos$Edad, na.rm = TRUE)    # Media
median(datos$Edad, na.rm = TRUE)  # Mediana
Mode(datos$Edad, na.rm = TRUE)    # Moda

#------------------------------------------------------------------------------
# 9. Medidas de dispersión
#------------------------------------------------------------------------------

sd(datos$Edad, na.rm = TRUE)      # Desviación estándar
var(datos$Edad, na.rm = TRUE)     # Varianza
range(datos$Edad, na.rm = TRUE)   # Rango (mínimo y máximo)
IQR(datos$Edad, na.rm = TRUE)     # Rango intercuartílico

#------------------------------------------------------------------------------
# 10. Boxplots
#------------------------------------------------------------------------------
# Boxplot simple
boxplot(datos$Edad, main = "Boxplot de la Edad", ylab = "Edad")

# Paso 1: Calcular los cuantiles (percentiles) de interés
# - probs = c(0, 0.25, 0.5, 0.75, 1): especifica los percentiles a calcular (mínimo, Q1, mediana, Q3, máximo)
# - na.rm = TRUE: ignora los valores NA en el cálculo

# color = "black",     # Color del borde del boxplot
# width = 0.5,         # Ancho del boxplot
# fill = "#3498DB"     # Color de relleno (azul)
# geom_boxplot: Crear el gráfico boxplot con ggplot2
# annotate: Añadir anotaciones con los valores de Q1, mediana y Q3

# Boxplot con anotaciones (cuantiles Q1, Mediana, Q3)
q <- quantile(datos$Edad, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
ggplot(datos, aes(y = Edad)) +
  geom_boxplot(color = "black", width = 0.5, fill = "#3498DB") +
  annotate("text", x = 1.2, y = q[2], label = paste0("Q1: ", q[2]), hjust = 1) +
  annotate("text", x = 1.2, y = q[3], label = paste0("Mediana: ", q[3]), hjust = 1) +
  annotate("text", x = 1.2, y = q[4], label = paste0("Q3: ", q[4]), hjust = 1) +
  labs(title = "Distribución de la edad con anotaciones", y = "Edad (años)") +
  theme_minimal()

# Boxplot por sexo
ggplot(datos, aes(x = Sexo, y = Edad, fill = Sexo)) +
  geom_boxplot(width = 0.2, color = "black") +
  scale_fill_manual(values = c("Femenino" = "#AD46FF", "Masculino" = "#3498DB")) +
  labs(title = "Distribución de la edad por sexo",
       y = "Edad (años)", x = "Sexo") +
  theme_minimal()

################################################################################
######################## Tablas y relaciones ###################################
################################################################################

#------------------------------------------------------------------------------
# 11. Tablas de frecuencias
#------------------------------------------------------------------------------
datos %>%
  count(Estado) %>%                        # Frecuencia absoluta
  mutate(pct = round(n/sum(n)*100, 1))     # Frecuencia relativa (%)

#------------------------------------------------------------------------------
# 12. Tablas cruzadas (ej: Sexo x Estado)
#------------------------------------------------------------------------------
datos %>%
  count(Sexo, Estado) %>%                  # Conteo combinado
  group_by(Sexo) %>%
  mutate(pct = round(n/sum(n)*100, 1))     # Porcentaje por sexo

################################################################################
######################## Tablas resumen automáticas ############################
################################################################################

#------------------------------------------------------------------------------
# 15. Tabla descriptiva general
#------------------------------------------------------------------------------
theme_gtsummary_journal(journal = "jama")

datos %>%
  select(Edad, Sexo, Tipo, Departamento, Estado) %>% # Selecciona solo las columnas de interés para la tabla resumen:
  tbl_summary( # GENERACIÓN DE TABLA RESUMEN
    statistic = all_continuous() ~ c(
      "{mean} ({sd})",         # Media y desviación estándar
      "{median} ({p25}, {p75})",# Mediana y rango intercuartílico
      "{min}, {max}"            # Mínimo y máximo
    ),
    type = all_continuous() ~ "continuous2"
  )

#------------------------------------------------------------------------------
# 16. Tabla descriptiva por sexo
#------------------------------------------------------------------------------
datos %>%
  select(Edad, Sexo, Tipo, Departamento, Estado) %>%
  tbl_summary(
    by = Sexo, # Desagregado por el Sexo
    statistic = all_continuous() ~ c(
      "{mean} ({sd})", "{median} ({p25}, {p75})", "{min}, {max}"
    ),
    type = all_continuous() ~ "continuous2"
  )

################################################################################
######################## Series temporales #####################################
################################################################################

#------------------------------------------------------------------------------
# 14. Casos a lo largo del tiempo
#------------------------------------------------------------------------------

# AGRUPACIÓN Y CONTEO -----------------------------------------
  # Cuenta cuántos casos hay por cada fecha de diagnóstico
  # - 'Fecha_de_diagnóstico' debe ser una variable de tipo fecha (Date)
  # - Se crea una columna 'n' con el número de casos por fecha
  
# Casos diarios
datos %>%
  count(Fecha_de_diagnóstico) %>%
  ggplot(aes(x = Fecha_de_diagnóstico, y = n)) +
  geom_line(color = "#3498DB") +
  labs(title = "Número de casos en el tiempo, Año 2020",
       x = "Fecha de diagnóstico", y = "Casos") +
  theme_minimal()

# Casos diarios desagregados por sexo
datos %>%
  group_by(Fecha_de_diagnóstico, Sexo) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = Fecha_de_diagnóstico, y = n, color = Sexo)) +
  geom_line(size = 1) +
  labs(title = "Número de casos en el tiempo por sexo, Año 2020",
       x = "Fecha de diagnóstico", y = "Casos", color = "Sexo") +
  theme_minimal()

################################################################################
######################## Pirámide poblacional ##################################
################################################################################

#------------------------------------------------------------------------------
# 15. Pirámide poblacional
#------------------------------------------------------------------------------
# Crear grupos de edad en intervalos de 5 años

# cut(): crea categorías a partir de una variable continua (Edad)
# breaks = seq(0, 100, by = 5): define los cortes en intervalos de 5 años desde 0 hasta 100
# right = FALSE: los intervalos son cerrados por la izquierda y abiertos por la derecha [0,5)
# labels = ...: genera etiquetas como "0-4", "5-9", ..., "95-99"

datos <- datos %>%
  mutate(GrupoEdad = cut(Edad, 
                         breaks = seq(0, 100, by = 5), 
                         right = FALSE,
                         labels = paste(seq(0, 95, by = 5), 
                                        seq(4, 99, by = 5), sep = "-")))

# Calcular frecuencias por grupo de edad y sexo
piramide <- datos %>%
  count(GrupoEdad, Sexo) %>% # Cuenta casos por combinación de grupo de edad y sexo
  mutate(n = ifelse(Sexo == "Masculino", -n, n))  # Negativos para graficar pirámide / # Esto permite que hombres se grafiquen hacia la izquierda y mujeres hacia la derecha

# Graficar pirámide
ggplot(piramide, aes(x = GrupoEdad, y = n, fill = Sexo)) +
  geom_bar(stat = "identity", width = 0.9) + # Barras de altura fija (valores exactos, no conteos automáticos)
  coord_flip() + # Invierte los ejes para que la edad esté en el eje Y (vertical)
  scale_y_continuous(labels = abs) +  # Mostrar valores en positivo
  scale_fill_manual(values = c("Femenino" = "#AD46FF", "Masculino" = "#3498DB")) +
  labs(title = "Pirámide poblacional de casos COVID-19",
       x = "Grupo de edad", y = "Número de casos") +
  theme_minimal()

################################################################################
######################## Mapa de casos por municipio ###########################
################################################################################

# Agrupar casos por municipio
casos_mpio <- datos %>%
  group_by(Ciudad, Código_ciudad) %>%
  summarise(Casos = n(), .groups = "drop") %>%
  mutate(Código_ciudad = as.numeric(Código_ciudad))  # Asegurar que sea numérico

# Leer shapefile de municipios (debe estar en la misma carpeta del script)
colombia <- st_read("Municipios_codigos.shp")

# Unir datos de casos al shapefile
mapa_join <- colombia %>%
  left_join(casos_mpio, by = c("COD_MPIO" = "Código_ciudad"))

# Graficar mapa con ggplot
ggplot(mapa_join) +
  # Dibujar polígonos de municipios, con color según número de casos
  geom_sf(aes(fill = Casos), color = "gray70", size = 0.2) +
  
  # Escala de color perceptual (viridis) — buena para informes médicos
  scale_fill_viridis_c(
    option = "plasma",      # Estilo de color (puedes probar "magma", "inferno", etc.)
    direction = -1,         # Invierte los colores para que valores altos sean más oscuros
    na.value = "gray90",    # Color para municipios sin datos
    name = "N° de casos"    # Etiqueta del gradiente
  ) +
  
  # Añadir título y etiquetas
  labs(
    title = "Distribución de casos por municipio",
    subtitle = "Fuente: Base de datos epidemiológica del INS",
    caption = "Elaborado por [Tu nombre o institución]"
  ) +
  
  # Mejorar el tema gráfico
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic", color = "gray40"),
    plot.caption = element_text(size = 9, color = "gray50", hjust = 1),
    legend.position = "right",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    axis.text = element_blank(),      # Quitar texto de ejes
    axis.ticks = element_blank(),     # Quitar marcas de ejes
    panel.grid = element_blank()      # Quitar grillas
  )

ggplot(mapa_join) +
  geom_sf(aes(fill = Casos), color = "gray60", size = 0.2) +
  scale_fill_distiller(
    palette = "YlOrRd",
    direction = 1,
    na.value = "gray90",
    name = "N° de casos"
  ) +
  labs(
    title = "Distribución de casos por municipio",
    subtitle = "Escala de color: menor (amarillo) → mayor (rojo)",
    caption = "Fuente: Base de datos epidemiológica"
  ) +
  theme_minimal(base_size = 12)
