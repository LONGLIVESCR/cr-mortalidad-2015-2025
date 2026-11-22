# =============================================================================
# Construcción de la base quinquenal de mortalidad
# Costa Rica, provincias, 2015-2025
# =============================================================================
#
# ENTRADA (dir_procesados, ver 00.0_config.R)
#   mortalidad_mx_1000_2015_2025.csv
#
# SALIDA (dir_procesados)
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

source(here::here("R", "00.0_config.R"))


# -----------------------------------------------------------------------------
# Configuración (tomada de 00.0_config.R)
# -----------------------------------------------------------------------------
# orden_provincias, orden_sexos, grupo_edad() y decimal_fijo() vienen de la
# config. grupo_edad() asigna el grupo quinquenal a cada edad simple.

archivo_entrada <- salida$edad_simple
archivo_salida  <- salida$quinquenal

orden_edades <- c(grupos_edad_quinquenal, "Ignorada")


# -----------------------------------------------------------------------------
# 1. Leer edad simple
# -----------------------------------------------------------------------------

validar_archivos(
  archivo_entrada,
  ayuda = "Corra antes R/edades_simples/01.0_generacion_bd_mx1000_2015_2025.R."
)

base_simple <- read_csv(archivo_entrada, show_col_types = FALSE)

nombre_entrada <- basename(archivo_entrada)
validar_columnas(
  base_simple,
  c("edad", "sexo", "anio", "region", "exposicion", "mortalidad"),
  nombre_entrada
)
validar_anios(base_simple$anio, nombre_entrada)
validar_valores(base_simple$sexo, orden_sexos, "sexo", nombre_entrada)
validar_valores(base_simple$region, orden_provincias, "region", nombre_entrada)
validar_valores(grupo_edad(base_simple$edad), orden_edades, "edad", nombre_entrada)


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
    salida$quinquenal_anio(anio_i)
  )
}
