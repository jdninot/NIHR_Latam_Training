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
# View(datos)  # (opcional) inspeccionar los datos en pestaña estilo Excel

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

################################################################################
######################## Tablas resumen automáticas ############################
################################################################################

#------------------------------------------------------------------------------
# 15. Tabla descriptiva general
#------------------------------------------------------------------------------
datos_seleccion <- datos %>%
  select(Estado, Sexo, Edad)

# Creamos la tabla resumen
tabla_resumen <- datos_seleccion %>%
  tbl_summary(    statistic = list(
      all_continuous() ~ "{mean} ({sd}) - Mediana: {median} (Q1: {p25}, Q3: {p75})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 2,
    label = list(
      Sexo ~ "Sexo",
      Edad ~ "Edad (años)",
      Estado ~ "Estado"
    )
  ) 
tabla_resumen

theme_gtsummary_journal(journal = "jama")

datos %>%
  tbl_summary( # GENERACIÓN DE TABLA RESUMEN
    statistic = all_conous() ~ c(
      "{mean} ({sd})",         # Media y desviación estándar
      "{median} ({p25}, {p75})",# Mediana y rango intercuartílico
      "{min}, {max}"            # Mínimo y máximo
    ),
    type = all_continuous() ~ "continuous2"
  )

# En R, realiza un tabla que me describa estas variables: Edad, Sexo, Tipo, Estado, preferiblemente con la libreria tbl_summary

datos %>%
  select(Edad, Sexo, Tipo, Estado) %>% # Selecciona solo las columnas de interés para la tabla resumen:
  tbl_summary( # GENERACIÓN DE TABLA RESUMEN
    statistic = all_continuous() ~ c(
      "{mean} ({sd})",         # Media y desviación estándar
      "{median} ({p25}, {p75})",# Mediana y rango intercuartílico
      "{min}, {max}"            # Mínimo y máximo
    ),
    type = all_continuous() ~ "continuous2"
  ) %>% 
  modify_header(label ~ "**Variable**") %>% 
  modify_caption("**Tabla 1. Característica  de los casos de COVID - 19**") %>% 
  bold_labels()

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

# Grafico mas avanzado
# Gráfico más avanzado con cambio de tipo de letra
datos %>%
  count(Fecha_de_diagnóstico) %>%
  ggplot(aes(x = Fecha_de_diagnóstico, y = n)) +
  geom_line(color = "#3498DB", size = 1) +
  geom_point(color = "#1B4F72", size = 2) +
  geom_smooth(method = "loess", se = FALSE, color = "#E74C3C", linetype = "dashed") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  labs(
    title = "Evolución de casos de COVID-19 hasta junio del 2020",
    subtitle = "Tendencia diaria de diagnósticos confirmados",
    x = "Fecha de diagnóstico",
    y = "Número de casos",
    caption = "Fuente: Base de datos del INS"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Roboto"),  # Cambia la fuente aquí
    plot.title = element_text(face = "bold", color = "red"),
    plot.subtitle = element_text(color = "#566573"),
    plot.caption = element_text(size = 9, color = "gray40")
  )


# Casos diarios desagregados por sexo
datos %>%
  group_by(Fecha_de_diagnóstico, Estado) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = Fecha_de_diagnóstico, y = n, color = Estado)) +
  geom_line(size = 1) +
  labs(
    title = "Evolución de casos de COVID-19 en 2020",
    subtitle = "Tendencia diaria de diagnósticos confirmados",
    x = "Fecha de diagnóstico",
    y = "Número de casos",
    caption = "Fuente: Base de datos del INS"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", color = "#2C3E50"),
    plot.subtitle = element_text(color = "#566573"),
    plot.caption = element_text(size = 9, color = "gray40")
  )

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

# Crear grupos de edad en intervalos de 5 años
datos <- datos %>%
  mutate(
    GrupoEdad = cut(
      Edad,
      breaks = seq(0, 100, by = 5),
      right = FALSE,
      labels = paste0(seq(0, 95, by = 5), "-", seq(4, 99, by = 5))
    )
  )
datos$GrupoEdad

# Calcular frecuencias por grupo de edad y sexo
piramide <- datos %>%
  count(GrupoEdad, Sexo, name = "Casos") %>%
  mutate(Casos = ifelse(Sexo == "Masculino", -Casos, Casos))

# Graficar pirámide poblacional
ggplot(piramide, aes(x = GrupoEdad, y = Casos, fill = Sexo)) +
  geom_bar(stat = "identity", width = 0.85, alpha = 0.9) +
  coord_flip() +
  scale_y_continuous(
    labels = function(x) abs(x),
    breaks = pretty_breaks(8)
  ) +
  scale_fill_manual(
    values = c("Femenino" = "#E67EFA", "Masculino" = "#3498DB"),
    name = "Sexo"
  ) +
  labs(
    title = "Pirámide poblacional de casos de COVID-19",
    subtitle = "Distribución de casos por edad y sexo",
    x = "Grupo de edad (años)",
    y = "Número de casos",
    caption = "Fuente: Base de datos del INS"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", color = "#2C3E50"),
    plot.subtitle = element_text(color = "#566573"),
    axis.text.x = element_text(color = "#2C3E50"),
    axis.text.y = element_text(color = "#2C3E50"),
    plot.caption = element_text(size = 9, color = "gray50"),
    legend.position = "top",
    legend.title = element_text(face = "bold")
  )

# Calcular porcentaje por grupo de edad y sexo
piramide <- datos %>%
  count(GrupoEdad, Sexo, name = "Casos") %>%
  group_by(Sexo) %>%
  mutate(Porcentaje = Casos / sum(Casos) * 100) %>%
  ungroup() %>%
  mutate(Porcentaje = ifelse(Sexo == "Masculino", -Porcentaje, Porcentaje))

# Invertir orden de grupos de edad (jóvenes abajo)
piramide <- piramide %>%
  mutate(GrupoEdad = factor(GrupoEdad, levels = rev(levels(GrupoEdad))))

# Graficar pirámide poblacional en porcentaje
ggplot(piramide, aes(x = GrupoEdad, y = Porcentaje, fill = Sexo)) +
  geom_bar(stat = "identity", width = 0.85, alpha = 0.9) +
  coord_flip() +
  scale_y_continuous(
    labels = function(x) paste0(abs(x), "%"),
    breaks = pretty_breaks(8)
  ) +
  scale_fill_manual(
    values = c("Femenino" = "#E67EFA", "Masculino" = "#3498DB"),
    name = "Sexo"
  ) +
  labs(
    title = "Pirámide poblacional de casos de COVID-19",
    subtitle = "Distribución porcentual de casos por edad y sexo",
    x = "Grupo de edad (años)",
    y = "Porcentaje de casos",
    caption = "Fuente: Base de datos del INS"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", color = "#2C3E50"),
    plot.subtitle = element_text(color = "#566573"),
    axis.text.x = element_text(color = "#2C3E50"),
    axis.text.y = element_text(color = "#2C3E50"),
    plot.caption = element_text(size = 9, color = "gray50"),
    legend.position = "top",
    legend.title = element_text(face = "bold")
  )

################################################################################
######################## Mapa de casos por municipio ###########################
################################################################################

# Agrupar casos por municipio
casos_mpio <- datos %>%
  select(Ciudad, Código_ciudad) %>% 
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
  geom_sf(aes(fill = Casos), color = "gray70") +
  
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
