# =============================================================================
# Verificar que la base quinquenal es la suma de la base de edad simple
# Costa Rica, provincias, 2015-2025
# =============================================================================
#
# Compara:
#   mortalidad_mx_1000_2015_2025.csv            edad simple
#   mortalidad_mx_1000_quinquenal_2015_2025.csv grupos 0, 1-4, 5-9, ...
#
# Una fila quinquenal está bien si:
#   exposicion quinquenal == suma de exposicion de las edades simples del grupo
#   mortalidad quinquenal == suma de mortalidad de las edades simples del grupo
#   mx             == mortalidad / exposicion
#   mx_1000        == mx * 1000
#
# Edad 0 se compara sola.
# Ignorada no tiene exposición: solo se compara el conteo de muertes.
# =============================================================================

library(dplyr)
library(readr)

# grupo_edad() viene de la config: es la misma regla de agrupación que se usó
# para construir la base quinquenal.
source(here::here("R", "00.0_config.R"))

archivo_simple     <- salida$edad_simple
archivo_quinquenal <- salida$quinquenal

validar_archivos(
  c(archivo_simple, archivo_quinquenal),
  ayuda = "Corra antes 01.0 y 02.0."
)

tolerancia_mx <- 1e-8


# -----------------------------------------------------------------------------
# 1. Leer
# -----------------------------------------------------------------------------

simple <- read_csv(archivo_simple, show_col_types = FALSE) %>%
  mutate(
    edad   = as.character(edad),
    sexo   = as.character(sexo),
    region = as.character(region),
    anio   = as.integer(anio)
  )

quinquenal <- read_csv(archivo_quinquenal, show_col_types = FALSE) %>%
  mutate(
    edad    = as.character(edad),
    sexo    = as.character(sexo),
    region  = as.character(region),
    anio    = as.integer(anio),
    mx      = as.numeric(mx),
    mx_1000 = as.numeric(mx_1000)
  )


# -----------------------------------------------------------------------------
# 2. Reconstruir la quinquenal desde la simple
# -----------------------------------------------------------------------------

esperada <- simple %>%
  mutate(edad = grupo_edad(edad)) %>%
  group_by(edad, sexo, anio, region) %>%
  summarise(
    exposicion_esp = sum(exposicion, na.rm = TRUE),
    mortalidad_esp = sum(mortalidad, na.rm = TRUE),
    edades_simples = n(),
    .groups = "drop"
  ) %>%
  mutate(
    exposicion_esp = if_else(edad == "Ignorada", NA_real_, exposicion_esp)
  )


# -----------------------------------------------------------------------------
# 3. Cruzar y marcar diferencias
# -----------------------------------------------------------------------------

comparacion <- quinquenal %>%
  full_join(
    esperada,
    by = c("edad", "sexo", "anio", "region")
  ) %>%
  mutate(
    diff_exposicion = if_else(
      is.na(exposicion) & is.na(exposicion_esp),
      0,
      exposicion - exposicion_esp
    ),
    diff_mortalidad = mortalidad - mortalidad_esp,
    mx_esp = if_else(
      is.na(exposicion_esp) | exposicion_esp == 0,
      NA_real_,
      mortalidad_esp / exposicion_esp
    ),
    mx_1000_esp = mx_esp * 1000,
    diff_mx = abs(mx - mx_esp),
    solo_en_quinquenal = is.na(mortalidad_esp),
    solo_en_simple     = is.na(mortalidad),
    ok_conteos = !solo_en_quinquenal &
      !solo_en_simple &
      abs(coalesce(diff_exposicion, 0)) < 0.5 &
      abs(coalesce(diff_mortalidad, 0)) < 0.5,
    ok_mx = is.na(mx_esp) | is.na(mx) | diff_mx < tolerancia_mx
  )


# -----------------------------------------------------------------------------
# 4. Resumen
# -----------------------------------------------------------------------------

cat("\n===== RESUMEN =====\n")
cat("Filas edad simple:     ", nrow(simple), "\n")
cat("Filas quinquenal:      ", nrow(quinquenal), "\n")
cat("Filas reconstruidas:   ", nrow(esperada), "\n")
cat("Coinciden en conteos:  ", sum(comparacion$ok_conteos, na.rm = TRUE), "\n")
cat("Solo en quinquenal:    ", sum(comparacion$solo_en_quinquenal, na.rm = TRUE), "\n")
cat("Solo en simple:        ", sum(comparacion$solo_en_simple, na.rm = TRUE), "\n")
cat("Exposición distinta:   ", sum(abs(comparacion$diff_exposicion) > 0.5, na.rm = TRUE), "\n")
cat("Mortalidad distinta:   ", sum(abs(comparacion$diff_mortalidad) > 0.5, na.rm = TRUE), "\n")
cat("mx distinto:           ", sum(!comparacion$ok_mx, na.rm = TRUE), "\n")

# Control interno: dentro de un grupo, 1-4 debe salir de 4 edades,
# un quinquenio de 5, y 0 de 1.
control_ancho <- esperada %>%
  filter(!edad %in% c("Ignorada", "90 años y más")) %>%
  mutate(
    ancho_esperado = case_when(
      edad == "0"   ~ 1L,
      edad == "1-4" ~ 4L,
      TRUE          ~ 5L
    )
  ) %>%
  filter(edades_simples != ancho_esperado)

cat("Grupos con ancho de edad raro:", nrow(control_ancho), "\n")


# -----------------------------------------------------------------------------
# 5. Detalle de lo que no cuadra
# -----------------------------------------------------------------------------

fallos <- comparacion %>%
  filter(!ok_conteos | !ok_mx) %>%
  select(
    edad, sexo, anio, region,
    exposicion, exposicion_esp, diff_exposicion,
    mortalidad, mortalidad_esp, diff_mortalidad,
    mx, mx_esp, mx_1000, mx_1000_esp
  ) %>%
  arrange(anio, region, sexo, edad)

if (nrow(fallos) == 0) {
  cat("\nOK. La quinquenal es la suma de la simple.\n")
} else {
  cat("\nFilas que no cuadran:", nrow(fallos), "\n")
  print(head(fallos, 30))
}

# Ejemplo visible: San José, Hombre, 2015, edad 0 y 1-4
cat("\n===== Ejemplo San José, Hombre, 2015 =====\n")
print(
  comparacion %>%
    filter(anio == 2015, region == "San José", sexo == "Hombre", edad %in% c("0", "1-4", "5-9")) %>%
    select(edad, exposicion, exposicion_esp, mortalidad, mortalidad_esp, mx, mx_esp, ok_conteos)
)

write_csv(
  fallos,
  salida$verif_quinquenal
)
cat("\nDetalle guardado en verificacion_quinquenal_fallos.csv\n")
