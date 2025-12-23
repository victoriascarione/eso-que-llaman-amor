# “Eso que llaman amor es trabajo no pago y desigualdad de ingresos”
 ## Autora: María Victoria Scarione Avellaneda.

rm(list = ls())

options(scipen=999)

# Cargo librerías
library(tidyverse)
library(openxlsx)
library(eph)
library(ggplot2)

# Levanto base individual
base_ind <- get_microdata(
  year = 2025,
  period = 2,
  type = "individual")

# Levanto base hogar
base_hogar <- get_microdata(
  year = 2025,
  period = 2,
  type = "hogar")

# Indicadores a construir
##1. Tasa de informalidad laboral por género.
##2. Brecha de ingresos per capita y por género.
##3. Subocupación por género.

# Variables:
  #CH03 → Relación de parentesco.
  #CH04 → Sexo (1 varón, 2 mujer).
  #ESTADO → Condición de actividad.
  #CAT_OCUP → Categoría ocupacional.
  #PP07H → ¿Por ese trabajo tiene descuento jubilatorio? 
  #PP3E_TOT → Total de horas que trabajó en la semana en la ocupación principal.
  #PP3F_TOT → Total de horas que trabajó en la semana en otras ocupaciones.
  #PP03G → La semana pasada, ¿quería trabajar más horas? 1 = Sí 2 = No
  #PONDIIO → Ponderador del ingreso de la ocupación principal.
  #P47T →  Monto de ingreso total individual (sumatoria ingresos laborales y no laborales).
  #IPCF →  Monto de ingreso per cápita familiar.
  #IX_Men10 → Cantidad de miembros del hogar menores de 10 años.
  #PONDIH → Ponderador del ingreso total familiar y del ingreso per cápita familiar.
  #PONDII →  Ponderador para ingreso total individual.

# Construyo el indicador: Tasa de informalidad por género.
##Selecciono sólo algunas columnas y renombro parte de ellas
eph_seleccion <- base_ind %>%
  select(CODUSU, NRO_HOGAR, COMPONENTE, ESTADO, EMPLEO,
         CAT_OCUP, CH04, CH06, PONDIIO)

## Filtro solo personas ocupadas y creo variable de género y de informalidad
eph_informal <- eph_seleccion %>%
  filter(ESTADO == 1) %>%
  mutate(
    genero = if_else(CH04 == 1, "Varón", "Mujer"),
    informal = if_else(EMPLEO == 2, 1, 0)  
  ) %>%
  left_join(
    base_hogar %>% select(CODUSU, NRO_HOGAR, PONDIH),
    by = c("CODUSU", "NRO_HOGAR")
  )

# Calculo tasa de informalidad por género
tasa_informalidad_genero <- eph_informal %>%
  group_by(genero) %>%
  summarise(
    tasa_informalidad = sum(informal * PONDIH, na.rm = TRUE) /
      sum(PONDIH, na.rm = TRUE)
  ) %>%
  mutate(tasa_informalidad = round(tasa_informalidad * 100, 1))

# Gráfico
graph_tasa_informalidad_genero <- ggplot(tasa_informalidad_genero, 
                                         aes(x = genero, y = tasa_informalidad, fill = genero)) +
  geom_col() +
  geom_text(    aes(label = paste0(round(tasa_informalidad, 1), "%")), vjust = -0.3, size = 4) +
    scale_fill_manual(values = c("Mujer" = "#CD1076", 
                               "Varón" = "#00008B")) +
  labs(
    title = "Tasa de informalidad por género",
    subtitle = "EPH – 2do trimestre. Año 2025.",
    x = "",
    y = "Porcentaje (%)",
    caption = "Fuente: elaboración propia en base a Encuesta Permanente de Hogares (EPH), INDEC. 2° trimestre 2025"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.background = element_rect(fill = "grey90", color = NA),
    plot.background = element_rect(fill = "grey90", color = NA)
  )
graph_tasa_informalidad_genero

# Construyo el indicador: Brecha de ingresos totales entre mujeres y varones
# Base individual con ingresos positivos
base_ing <- base_ind %>%
  mutate(
    genero = if_else(CH04 == 1, "Varón", "Mujer"),
    P47T   = as.numeric(P47T),
    PONDII = as.numeric(PONDII)
  ) %>%
  filter(!is.na(P47T), P47T > 0,
         !is.na(PONDII), PONDII > 0)

# Promedio ponderado de ingresos totales por género
ing_prom <- base_ing %>%
  group_by(genero) %>%
  summarise(
    ingreso = weighted.mean(P47T, PONDII, na.rm = TRUE)
  )

print(ing_prom)

# Estimo brecha de ingresos
ing_varon <- ing_prom$ingreso[ing_prom$genero == "Varón"]
ing_mujer <- ing_prom$ingreso[ing_prom$genero == "Mujer"]

brecha <- (1 - ing_mujer / ing_varon) * 100
brecha

# Grafico
graph_ing_prom <- ggplot(ing_prom, aes(x = genero, y = ingreso, fill = genero)) +
  geom_col() +
  geom_text(aes(label = round(ingreso, 0)), vjust = -0.5) +
  scale_fill_manual(values = c("Mujer" = "#CD1076", "Varón" = "#00008B")) +
  labs(
    title = "Ingreso total individual promedio por género",
    subtitle = paste0("Brecha: ", round(brecha, 1), "%"),
    x = "", 
    y = "ARS",
    caption = "Fuente: elaboración propia en base a Encuesta Permanente de Hogares (EPH), INDEC. 2° trimestre 2025"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.background = element_rect(fill = "grey90", color = NA),
    plot.background  = element_rect(fill = "grey90", color = NA)
  )

graph_ing_prom


# Construyo el indicador: Subocupación por género.
# Creo variable de género
base_ind <- base_ind %>%
  mutate(
    genero = if_else(CH04 == 1, "Varón", "Mujer")
  )

# Defino subocupación (subocupadxs demandantes)
subocupacion <- base_ind %>%
  filter(ESTADO == 1) %>% 
  mutate(
    sub_no_vol = if_else(
      PP3E_TOT < 35 & PP03G == 1 & PP03H == 1,
      1, 0
    )
  )

# Calculo tasa por género
tasa_sub <- subocupacion %>%
  group_by(genero) %>%
  summarise(
    tasa_sub = sum(sub_no_vol * PONDIIO, na.rm = TRUE) /
      sum(PONDIIO, na.rm = TRUE)
  ) %>%
  mutate(tasa_sub = tasa_sub * 100)

tasa_sub

# Grafico
graph_tasa_sub <- ggplot(tasa_sub, aes(x = genero, y = tasa_sub, fill = genero)) +
  geom_col() +
  geom_text(
    aes(label = paste0(round(tasa_sub, 1), "%")),
    vjust = -0.4
  ) +
  scale_fill_manual(values = c("Mujer" = "#CD1076", "Varón" = "#00008B")) +
  labs(
    title = "Subocupación por género",
    subtitle = "EPH – 2º trimestre 2025",
    x = "",
    y = "Porcentaje (%)",
    caption = "Fuente: elaboración propia en base a Encuesta Permanente de Hogares (EPH), INDEC. 2° trimestre 2025"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.background = element_rect(fill = "grey90", color = NA),
    plot.background = element_rect(fill = "grey90", color = NA)
  )

graph_tasa_sub
