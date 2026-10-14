# =============================================================================
# Unificar la base de mortalidad con el diccionario INEC - CIE10
#
# - La base (salida de 04) trae INEC_4: el código numérico REDATAM causa4dig
#   (104, 921, 1044...). Desde el script 03 ya se llama así.
# - Se agrega a la par la columna CIE10_4 (categoría CIE-10 de 3 caracteres:
#   A04, I21, J44...), tomada del diccionario según INEC_4 y año.
# - Un código sin pareja en el diccionario de su año detiene el script
# =============================================================================

library(dplyr)
library(readr)

source("R/00.0_config.R")

archivo_base <- salida$cie10_mx
archivo_dic  <- salida$diccionario
archivo_sal  <- salida$unificada

base <- read_csv(archivo_base, col_types = cols(.default = col_character()))
dic  <- read_csv(archivo_dic, col_types = cols(codigo_redatam = col_character(),
                                               INEC_4 = col_integer(),
                                               anio   = col_integer(),
                                               .default = col_character()))

# ---- 1. Revisar la base -----------------------------------------------------
if ("CIE10_4" %in% names(base)) {
  stop("La base ya tiene CIE10_4. Si es una salida vieja de 03/04, vuelva a correr 03 y 04; ",
       "si ya se corrió este script, use la salida original de 04.")
}
if (!"INEC_4" %in% names(base)) stop("La base no tiene la columna INEC_4 (salida de 03/04).")
if (!all(grepl("^[0-9]{1,4}$", base$INEC_4))) stop("INEC_4 de la base no es el código numérico INEC; revisa el archivo.")

base <- base |>
  mutate(INEC_4 = as.integer(INEC_4), anio = as.integer(anio))

valores_antes <- base   # para comprobar que nada cambió

# ---- 2. Diccionario: una fila por año e INEC_4 ------------------------------
dic_cat <- dic |> distinct(anio, INEC_4, CIE10_4)

dobles <- dic_cat |> count(anio, INEC_4) |> filter(n > 1)
if (nrow(dobles) > 0) { print(dobles); stop("Algún INEC_4 tiene más de una CIE10_4 en el diccionario.") }

# ---- 3. Unir y poner CIE10_4 a la par de INEC_4 -----------------------------
filas_antes <- nrow(base)
total_antes <- sum(as.numeric(base$mortalidad), na.rm = TRUE)

unida <- base |>
  left_join(dic_cat, by = c("anio", "INEC_4"), relationship = "many-to-one") |>
  relocate(CIE10_4, .after = INEC_4)

# Códigos de la base que no están en la edición de su año
sin_pareja <- unida |>
  filter(is.na(CIE10_4)) |>
  count(anio, INEC_4, name = "filas")

if (nrow(sin_pareja) > 0) {
  cat("\nCódigos sin pareja en el diccionario de su año:\n")
  print(sin_pareja |> left_join(excepciones_inec4, by = c("anio", "INEC_4")))

  no_esperados <- sin_pareja |> anti_join(excepciones_inec4, by = c("anio", "INEC_4"))
  if (nrow(no_esperados) > 0) {
    print(no_esperados)
    stop("Hay códigos sin pareja en el diccionario que no están en excepciones_inec4 ",
         "(R/00_config.R). Revíselos antes de agregarlos como excepción.")
  }

  # Solo las excepciones documentadas se completan con la regla 01=A ... 26=Z.
  unida <- unida |>
    mutate(CIE10_4 = if_else(is.na(CIE10_4), inec4_a_cie10(INEC_4), CIE10_4))
}

# ---- 4. Verificaciones ------------------------------------------------------
stopifnot(nrow(unida) == filas_antes)                                  # no se duplicaron filas
stopifnot(isTRUE(all.equal(sum(as.numeric(unida$mortalidad), na.rm = TRUE), total_antes)))  # mismas defunciones
# edad, sexo, anio, region, INEC_4, mortalidad, exposicion, mx y mx_1000 idénticos
stopifnot(isTRUE(all.equal(as.data.frame(unida |> select(all_of(names(valores_antes)))),
                           as.data.frame(valores_antes), check.attributes = FALSE)))
stopifnot(!any(is.na(unida$CIE10_4)))
# La categoría del diccionario coincide con la regla (inec4_a_cie10 detiene el
# script si alguna letra queda fuera de 01-26).
stopifnot(all(unida$CIE10_4 == inec4_a_cie10(unida$INEC_4)))
cat("\nOK: mismas filas (", filas_antes, ") y mismas defunciones (", total_antes, ").\n", sep = "")

cat("\nEjemplos:\n")
print(unida |> filter(INEC_4 %in% c(104, 514, 921, 1044, 2107)) |>
        distinct(INEC_4, CIE10_4) |> arrange(INEC_4))

# ---- 5. Guardar -------------------------------------------------------------
write_csv(unida, archivo_sal)
cat("\nGuardado:", archivo_sal, "\n")
print(head(unida))
