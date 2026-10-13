# =============================================================================
# Verificar mortalidad_cie10_mx_2015_2025.csv
# =============================================================================

library(dplyr)
library(readr)

options(scipen = 999)

carpeta <- "dirección de la carpeta"

causas <- read_csv(
  file.path(carpeta, "mortalidad_cie10_2015_2025.csv"),
  show_col_types = FALSE
) %>%
  mutate(edad = if_else(edad == "Edad ignorada", "Ignorada", as.character(edad)))

mx_base <- read_csv(
  file.path(carpeta, "mortalidad_mx_1000_quinquenal_2015_2025.csv"),
  show_col_types = FALSE
) %>%
  filter(sexo %in% c("Hombre", "Mujer")) %>%
  transmute(
    edad = as.character(edad),
    sexo = as.character(sexo),
    anio = as.integer(anio),
    region = as.character(region),
    exposicion_esp = as.numeric(exposicion),
    mortalidad_total = as.numeric(mortalidad)
  )

final <- read_csv(
  file.path(carpeta, "mortalidad_cie10_mx_2015_2025.csv"),
  show_col_types = FALSE
) %>%
  mutate(
    edad = as.character(edad),
    sexo = as.character(sexo),
    region = as.character(region),
    anio = as.integer(anio),
    mx = as.numeric(mx),
    mx_1000 = as.numeric(mx_1000)
  )

cat("Filas causas: ", nrow(causas), "\n")
cat("Filas final:  ", nrow(final), "\n")
cat("Misma cantidad de filas: ", nrow(causas) == nrow(final), "\n")
cat("Quedó 'Edad ignorada': ", any(final$edad == "Edad ignorada", na.rm = TRUE), "\n")

exposicion_unica <- final %>%
  filter(edad != "Total") %>%
  group_by(edad, sexo, anio, region) %>%
  summarise(n_exp = n_distinct(exposicion), .groups = "drop")

cat("Grupos con más de una exposición: ", sum(exposicion_unica$n_exp > 1), "\n")

cruce <- final %>%
  filter(edad != "Total") %>%
  distinct(edad, sexo, anio, region, exposicion) %>%
  full_join(mx_base, by = c("edad", "sexo", "anio", "region")) %>%
  mutate(
    diff_exp = if_else(
      is.na(exposicion) & is.na(exposicion_esp),
      0,
      exposicion - exposicion_esp
    )
  )

cat("Exposición distinta: ", sum(abs(cruce$diff_exp) > 0.5, na.rm = TRUE), "\n")
cat("Sin pareja en quinquenal: ", sum(is.na(cruce$exposicion_esp)), "\n")
cat("Sin pareja en causas: ", sum(is.na(cruce$exposicion)), "\n")

tasas <- final %>%
  mutate(
    mx_esp = if_else(is.na(exposicion) | exposicion == 0, NA_real_, mortalidad / exposicion),
    ok_mx = is.na(mx_esp) | abs(mx - mx_esp) < 1e-8,
    ok_1000 = is.na(mx_esp) | abs(mx_1000 - mx_esp * 1000) < 1e-6
  )

cat("mx mal calculado: ", sum(!tasas$ok_mx, na.rm = TRUE), "\n")
cat("mx_1000 mal calculado: ", sum(!tasas$ok_1000, na.rm = TRUE), "\n")

suma_causas <- final %>%
  filter(edad != "Total", edad != "Ignorada") %>%
  group_by(edad, sexo, anio, region) %>%
  summarise(muertes_causas = sum(mortalidad), .groups = "drop") %>%
  left_join(mx_base, by = c("edad", "sexo", "anio", "region")) %>%
  mutate(diff = muertes_causas - mortalidad_total)

cat("Grupos donde las causas superan el total: ", sum(suma_causas$diff > 0.5, na.rm = TRUE), "\n")
cat("Diferencia máxima causas - total: ", max(suma_causas$diff, na.rm = TRUE), "\n")

cat("\n===== Ejemplo San José, Hombre, 2015, edad 0 =====\n")
print(
  final %>%
    filter(anio == 2015, region == "San José", sexo == "Hombre", edad == "0", mortalidad > 0) %>%
    select(CIE10_4, mortalidad, exposicion, mx, mx_1000) %>%
    head(8)
)

if (
  nrow(causas) == nrow(final) &&
  sum(exposicion_unica$n_exp > 1) == 0 &&
  sum(abs(cruce$diff_exp) > 0.5, na.rm = TRUE) == 0 &&
  sum(!tasas$ok_mx, na.rm = TRUE) == 0
) {
  cat("\nOK. La exposición se repitió bien y mx sale de la mortalidad por causa.\n")
}
