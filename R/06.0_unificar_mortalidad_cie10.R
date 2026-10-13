# =============================================================================
# Unificar la base de mortalidad con el diccionario INEC - CIE-10
#
# - En la base, la columna CIE10_4 trae en realidad el código INEC numérico
#   (102, 921, 1044...). Se renombra a INEC_4.
# - Se agrega a la par la columna CIE10_4 correcta (A02, I21, J44...), tomada
#   del diccionario según INEC_4 y año.
# - No se duplican filas: el diccionario se reduce a una fila por año e INEC_4
#   antes de unir.
# =============================================================================

library(dplyr)
library(readr)

carpeta_datos <- "dirección de la carpeta con los datos procesados"
archivo_base  <- file.path(carpeta_datos, "mortalidad_cie10_mx_2015_2025.csv")
archivo_dic   <- file.path(carpeta_datos, "diccionario_inec_cie10_por_anio.csv")
archivo_sal   <- file.path(carpeta_datos, "mortalidad_inec_cie10_mx_2015_2025.csv")

# Todo se lee como texto para no alterar ningún valor: mx y mx_1000 vienen con
# 10 decimales fijos (decimal_fijo en 04_unir_cie10_exposicion.R) y si se leen
# como número, write_csv los reescribe con otro formato (p. ej. 6.4612e-05).
# Este script NO recalcula mx ni mx_1000: solo renombra y agrega CIE10_4.
base <- read_csv(archivo_base, col_types = cols(.default = col_character()))
dic  <- read_csv(archivo_dic, col_types = cols(codigo_redatam = col_character(),
                                               INEC_4 = col_integer(),
                                               anio   = col_integer(),
                                               .default = col_character()))

# ---- 1. Renombrar CIE10_4 -> INEC_4 en la base ------------------------------
if (!"CIE10_4" %in% names(base)) stop("La base no tiene la columna CIE10_4.")
if ("INEC_4" %in% names(base))   stop("La base ya tiene INEC_4; ¿ya se corrió este script?")
if (!all(grepl("^[0-9]{1,4}$", base$CIE10_4))) stop("CIE10_4 de la base no es el código numérico INEC; revisa el archivo.")

base <- base |>
  rename(INEC_4 = CIE10_4) |>
  mutate(INEC_4 = as.integer(INEC_4), anio = as.integer(anio))

valores_antes <- base |> select(-INEC_4)   # para comprobar que nada cambió

# ---- 2. Diccionario: una fila por año e INEC_4 ------------------------------
dic_cat <- dic |> distinct(anio, INEC_4, CIE10_4)

dobles <- dic_cat |> count(anio, INEC_4) |> filter(n > 1)
if (nrow(dobles) > 0) { print(dobles); stop("Algún INEC_4 tiene más de una CIE10_4 en el diccionario.") }

# ---- 3. Unir y poner CIE10_4 a la par de INEC_4 -----------------------------
filas_antes <- nrow(base)
total_antes <- sum(as.numeric(base$mortalidad), na.rm = TRUE)

unida <- base |>
  left_join(dic_cat, by = c("anio", "INEC_4")) |>
  relocate(CIE10_4, .after = INEC_4)

# Códigos de la base que no están en la edición de su año
sin_pareja <- unida |> filter(is.na(CIE10_4))
if (nrow(sin_pareja) > 0) {
  cat("\nCódigos sin pareja en el diccionario de su año:\n")
  print(sin_pareja |>
          group_by(anio, INEC_4) |>
          summarise(filas = n(), .groups = "drop") |>
          mutate(CIE10_4_calculada = paste0(LETTERS[INEC_4 %/% 100], sprintf("%02d", INEC_4 %% 100))))
  # Se completan con la misma regla del diccionario (01=A ... 26=Z).
  # Esperado: 1009 (J09, influenza pandémica) en 2015-2016, que no existe en Ed2008.
  unida <- unida |>
    mutate(CIE10_4 = if_else(is.na(CIE10_4),
                             paste0(LETTERS[INEC_4 %/% 100], sprintf("%02d", INEC_4 %% 100)),
                             CIE10_4))
}

# ---- 4. Verificaciones ------------------------------------------------------
stopifnot(nrow(unida) == filas_antes)                                  # no se duplicaron filas
stopifnot(isTRUE(all.equal(sum(as.numeric(unida$mortalidad), na.rm = TRUE), total_antes)))  # mismas defunciones
# mortalidad, exposicion, mx y mx_1000 idénticos (mismo texto, mismo orden)
stopifnot(isTRUE(all.equal(as.data.frame(unida |> select(all_of(names(valores_antes)))),
                           as.data.frame(valores_antes), check.attributes = FALSE)))
stopifnot(!any(is.na(unida$CIE10_4)))
stopifnot(all(unida$CIE10_4 == paste0(LETTERS[unida$INEC_4 %/% 100],
                                      sprintf("%02d", unida$INEC_4 %% 100))))
cat("\nOK: mismas filas (", filas_antes, ") y mismas defunciones (", total_antes, ").\n", sep = "")

cat("\nEjemplos:\n")
print(unida |> filter(INEC_4 %in% c(104, 514, 921, 1044, 2107)) |>
        distinct(INEC_4, CIE10_4) |> arrange(INEC_4))

# ---- 5. Guardar -------------------------------------------------------------
write_csv(unida, archivo_sal)
cat("\nGuardado:", archivo_sal, "\n")
print(head(unida))