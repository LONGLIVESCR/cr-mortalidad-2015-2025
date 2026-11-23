# =============================================================================
# Unir causas CIE-10 con la exposición quinquenal
# =============================================================================
#
# Entrada (dir_procesados, ver 00.0_config.R)
#   mortalidad_cie10_2015_2025.csv
#     edad, sexo, anio, region, INEC_4, mortalidad
#   mortalidad_mx_1000_quinquenal_2015_2025.csv
#     solo se usa exposicion
#
# Salida
#   mortalidad_cie10_mx_2015_2025.csv
#     edad, sexo, anio, region, INEC_4, mortalidad, exposicion, mx, mx_1000
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

# orden_provincias y decimal_fijo() vienen de la config.
source(here::here("R", "00.0_config.R"))

archivo_cie <- salida$cie10
archivo_mx  <- salida$quinquenal
archivo_out <- salida$cie10_mx

orden_edades <- c(grupos_edad_quinquenal, "Ignorada", "Total")

validar_archivos(c(archivo_cie, archivo_mx), ayuda = "Corra antes 02.0 y 03.0.")


# -----------------------------------------------------------------------------
# 1. Causas
# -----------------------------------------------------------------------------

causas <- read_csv(archivo_cie, show_col_types = FALSE)

validar_columnas(
  causas,
  c("edad", "sexo", "anio", "region", "INEC_4", "mortalidad"),
  basename(archivo_cie)
)
validar_anios(causas$anio, basename(archivo_cie))

causas <- causas %>%
  mutate(
    edad   = if_else(edad == "Edad ignorada", "Ignorada", as.character(edad)),
    sexo   = as.character(sexo),
    region = as.character(region),
    anio   = as.integer(anio)
  )


# -----------------------------------------------------------------------------
# 2. Solo la exposición
# -----------------------------------------------------------------------------

exposicion <- read_csv(archivo_mx, show_col_types = FALSE)

validar_columnas(
  exposicion,
  c("edad", "sexo", "anio", "region", "exposicion"),
  basename(archivo_mx)
)

exposicion <- exposicion %>%
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
    region  = factor(region, levels = orden_provincias)
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
