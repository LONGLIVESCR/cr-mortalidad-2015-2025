# =============================================================================
# Construcción de la base quinquenal de mortalidad
# Costa Rica, provincias, 2015-2025
# =============================================================================
#
# ENTRADA
#   mortalidad_mx_1000_2015_2025.csv
#
# SALIDA
#   mortalidad_mx_1000_quinquenal_2015_2025.csv
#   Un archivo por año: mortalidad_mx_1000_quinquenal_YYYY.csv
#
# COMPOSICIÓN DE LA TABLA NUEVA
# -----------------------------------------------------------------------------
# columna      tipo        contenido
# edad         texto       grupo de edad
#                          0 | 1-4 | 5-9 | ... | 85-89 | 90 años y más | Ignorada
# sexo         texto       Hombre | Mujer | Total
# anio         entero      año de registro (2015-2025)
# region       texto       provincia de residencia
# exposicion   entero      población del grupo (suma de las edades simples)
# mortalidad   entero      defunciones del grupo (suma de las edades simples)
# mx           decimal     mortalidad / exposicion
# mx_1000      decimal     mx * 1000
#
# REGLAS DE AGREGACIÓN
#   - Edad 0 no se mezcla con 1-4.
#   - 1, 2, 3 y 4 se suman en 1-4.
#   - Desde 5 hasta 89: grupos de 5 años.
#   - 90 años y más se mantiene.
#   - Ignorada se mantiene; no tiene exposición comparable.
#   - mx se recalcula después de sumar.
# =============================================================================

library(dplyr)
library(readr)

options(scipen = 999)


# -----------------------------------------------------------------------------
# Configuración
# -----------------------------------------------------------------------------

carpeta <- "dirección de la carpeta"

archivo_entrada <- file.path(carpeta, "mortalidad_mx_1000_2015_2025.csv")
archivo_salida  <- file.path(carpeta, "mortalidad_mx_1000_quinquenal_2015_2025.csv")

orden_provincias <- c(
  "San José", "Alajuela", "Cartago", "Heredia",
  "Guanacaste", "Puntarenas", "Limón"
)

orden_edades <- c(
  "0", "1-4",
  "5-9", "10-14", "15-19", "20-24", "25-29",
  "30-34", "35-39", "40-44", "45-49", "50-54",
  "55-59", "60-64", "65-69", "70-74", "75-79",
  "80-84", "85-89",
  "90 años y más",
  "Ignorada"
)

orden_sexos <- c("Hombre", "Mujer", "Total")


# -----------------------------------------------------------------------------
# Asignar grupo de edad
# -----------------------------------------------------------------------------

grupo_edad <- function(edad) {

  edad <- as.character(edad)
  edad_num <- suppressWarnings(as.integer(edad))

  case_when(
    edad == "Ignorada"      ~ "Ignorada",
    edad == "90 años y más" ~ "90 años y más",
    edad == "0"             ~ "0",
    edad_num %in% 1:4       ~ "1-4",
    !is.na(edad_num) & edad_num >= 5 & edad_num <= 89 ~
      paste0(5 * (edad_num %/% 5), "-", 5 * (edad_num %/% 5) + 4),
    TRUE ~ edad
  )
}


# -----------------------------------------------------------------------------
# Escribir tasas en decimal fijo 
# -----------------------------------------------------------------------------

decimal_fijo <- function(x, decimales = 10) {
  if_else(is.na(x), NA_character_, sprintf(paste0("%.", decimales, "f"), x))
}


# -----------------------------------------------------------------------------
# 1. Leer edad simple
# -----------------------------------------------------------------------------

base_simple <- read_csv(archivo_entrada, show_col_types = FALSE)


# -----------------------------------------------------------------------------
# 2. Agrupa y recalcula mx
# -----------------------------------------------------------------------------

base_quinquenal <- base_simple %>%
  mutate(edad = grupo_edad(edad)) %>%
  group_by(edad, sexo, anio, region) %>%
  summarise(
    exposicion = sum(exposicion, na.rm = TRUE),
    mortalidad = sum(mortalidad, na.rm = TRUE),
    .groups    = "drop"
  ) %>%
  mutate(
    exposicion = if_else(edad == "Ignorada", NA_real_, as.numeric(exposicion)),
    mx         = if_else(
      is.na(exposicion) | exposicion == 0,
      NA_real_,
      mortalidad / exposicion
    ),
    mx_1000    = mx * 1000,
    edad       = factor(edad,   levels = orden_edades),
    sexo       = factor(sexo,   levels = orden_sexos),
    region     = factor(region, levels = orden_provincias)
  ) %>%
  arrange(anio, region, sexo, edad) %>%
  select(edad, sexo, anio, region, exposicion, mortalidad, mx, mx_1000)


# -----------------------------------------------------------------------------
# 3. Exportar
# -----------------------------------------------------------------------------

base_export <- base_quinquenal %>%
  mutate(
    mx      = decimal_fijo(mx),
    mx_1000 = decimal_fijo(mx_1000)
  )

write_csv(base_export, archivo_salida)

for (anio_i in sort(unique(base_quinquenal$anio))) {
  write_csv(
    filter(base_export, anio == anio_i),
    file.path(carpeta, paste0("mortalidad_mx_1000_quinquenal_", anio_i, ".csv"))
  )
}
