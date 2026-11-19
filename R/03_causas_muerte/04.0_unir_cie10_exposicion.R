# =============================================================================
# Unir causas CIE-10 con la exposición quinquenal
# =============================================================================
#
# Entrada (carpeta bd/bdProcces)
#   mortalidad_cie10_2015_2025.csv
#     edad, sexo, anio, region, CIE10_4, mortalidad
#   mortalidad_mx_1000_quinquenal_2015_2025.csv
#     solo se usa exposicion
#
# Salida
#   mortalidad_cie10_mx_2015_2025.csv
#     edad, sexo, anio, region, CIE10_4, mortalidad, exposicion, mx, mx_1000
#
# Reglas
#   - "Edad ignorada" se llama "Ignorada".
#   - La exposición se repite en cada CIE de la misma edad, sexo, año y provincia.
#   - No se trae la mortalidad ni el mx de la base quinquenal.
#   - mx = mortalidad / exposicion
#   - mx_1000 = mx * 1000
#   - La fila de edad "Total" no existe en la quinquenal.
#     Su exposición es la suma de las edades con población se excluye Ignorada.
# =============================================================================

library(dplyr)
library(readr)

options(scipen = 999)

carpeta <- "dirección de la carpeta"

archivo_cie <- file.path(carpeta, "mortalidad_cie10_2015_2025.csv")
archivo_mx  <- file.path(carpeta, "mortalidad_mx_1000_quinquenal_2015_2025.csv")
archivo_out <- file.path(carpeta, "mortalidad_cie10_mx_2015_2025.csv")

orden_regiones <- c(
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
  "Ignorada",
  "Total"
)

decimal_fijo <- function(x, decimales = 10) {
  if_else(is.na(x), NA_character_, sprintf(paste0("%.", decimales, "f"), x))
}


# -----------------------------------------------------------------------------
# 1. Causas
# -----------------------------------------------------------------------------

causas <- read_csv(archivo_cie, show_col_types = FALSE) %>%
  mutate(
    edad   = if_else(edad == "Edad ignorada", "Ignorada", as.character(edad)),
    sexo   = as.character(sexo),
    region = as.character(region),
    anio   = as.integer(anio)
  )


# -----------------------------------------------------------------------------
# 2. Solo la exposición
# -----------------------------------------------------------------------------

exposicion <- read_csv(archivo_mx, show_col_types = FALSE) %>%
  filter(sexo %in% c("Hombre", "Mujer")) %>%
  transmute(
    edad       = as.character(edad),
    sexo       = as.character(sexo),
    anio       = as.integer(anio),
    region     = as.character(region),
    exposicion = as.numeric(exposicion)
  )

exposicion_total <- exposicion %>%
  filter(edad != "Ignorada") %>%
  group_by(sexo, anio, region) %>%
  summarise(exposicion = sum(exposicion, na.rm = TRUE), .groups = "drop") %>%
  mutate(edad = "Total")

exposicion <- bind_rows(exposicion, exposicion_total)


# -----------------------------------------------------------------------------
# 3. Pegar exposición y calcular tasas
# -----------------------------------------------------------------------------

base_final <- causas %>%
  left_join(exposicion, by = c("edad", "sexo", "anio", "region")) %>%
  mutate(
    mx = if_else(
      is.na(exposicion) | exposicion == 0,
      NA_real_,
      mortalidad / exposicion
    ),
    mx_1000 = mx * 1000,
    edad    = factor(edad, levels = orden_edades),
    sexo    = factor(sexo, levels = c("Hombre", "Mujer")),
    region  = factor(region, levels = orden_regiones)
  ) %>%
  arrange(anio, region, as.integer(INEC_4), edad, sexo) %>%
  select(edad, sexo, anio, region, INEC_4, mortalidad, exposicion, mx, mx_1000)

base_export <- base_final %>%
  mutate(
    mx      = decimal_fijo(mx),
    mx_1000 = decimal_fijo(mx_1000)
  )

write_csv(base_export, archivo_out)

cat("Filas:", nrow(base_export), "\n")
cat("Sin exposición:", sum(is.na(base_final$exposicion)), "\n")
cat("Archivo:", archivo_out, "\n")
print(head(filter(base_export, mortalidad > 0), 8))
