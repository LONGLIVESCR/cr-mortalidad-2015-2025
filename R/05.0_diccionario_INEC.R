# =============================================================================
# Diccionario del Excel del INEC con el código CIE-10 separado del nombre.
# Columnas: codigo_redatam, INEC_4, CIE10_4, CIE10, nombre_cie10, anio.
# Una fila por subcategoría (CIE10). Para unir con la base de mortalidad se usa
# INEC_4 (o CIE10_4) + anio; cada INEC_4 trae varias filas.
#
# 2015-2016  → lista Ed2008
# 2017-2020  → lista Ed2015
# 2021-2025  → lista 2021 en adelante
#
# =============================================================================

library(readxl)
library(dplyr)
library(readr)

carpeta_excel <- "dirección de la carpeta con los archivos Excel"
carpeta_datos <- "dirección de la carpeta con los datos procesados"

listas <- data.frame(
  archivo = c(
    "Causa de muerte CIE-10 Ed2008 utilizada para período 2000-2016.xlsx",
    "Causa de muerte CIE-10 Ed2015 utilizada del 2017-2020.xlsx",
    "Grupos Causa de Muerte 2021 en adelante.xlsx"
  ),
  anio_min = c(2015L, 2017L, 2021L),
  anio_max = c(2016L, 2020L, 2025L),
  stringsAsFactors = FALSE
)

leer_lista <- function(ruta) {
  raw <- read_excel(
    ruta,
    sheet     = "Causas",
    col_names = FALSE,
    col_types = "text",   # texto: no se pierden ceros iniciales
    .name_repair = "minimal"
  )
  raw <- raw[, c(1, 3)]
  names(raw) <- c("codigo_redatam", "causa_completa")

  raw |>
    mutate(codigo_redatam = trimws(codigo_redatam)) |>
    # Solo filas de datos: código numérico de 4-5 dígitos y nombre presente.
    filter(grepl("^[0-9]{4,5}$", codigo_redatam), !is.na(causa_completa)) |>
    mutate(codigo_redatam = formatC(as.integer(codigo_redatam), width = 5, flag = "0")) |>
    separar_cie10()
}

# Separa "A040-Infección debida a ..." y agrega las llaves para unir con la base:
# - INEC_4:       primeros 4 dígitos del código, como número (01040 -> 104).
#                 Es el mismo valor que trae la base de mortalidad (causa4dig).
# - CIE10_4:      categoría CIE-10 de 3 caracteres que corresponde a INEC_4 (A04).
#                 Se llama "_4" porque viene del código INEC de 4 dígitos.
# - CIE10:        subcategoría CIE-10 de 4 caracteres (A040), calculada desde
#                 codigo_redatam (01=A ... 26=Z). Así no se copian los errores de
#                 escritura del INEC (03818 viene rotulado "C819", pero es C818).

separar_cie10 <- function(df) {
  m <- regmatches(df$causa_completa,
                  regexec("^\\s*[A-Z][0-9]{2}[0-9X]?\\s*-\\s*(.*)$", df$causa_completa))
  df$nombre_cie10 <- vapply(m, function(x) if (length(x) == 2) trimws(x[2]) else NA_character_, "")
  if (any(is.na(df$nombre_cie10))) {
    print(df[is.na(df$nombre_cie10), c("codigo_redatam", "causa_completa")])
    stop("Hay nombres que no se pudieron separar.")
  }
  letra      <- LETTERS[as.integer(substr(df$codigo_redatam, 1, 2))]
  df$INEC_4  <- as.integer(substr(df$codigo_redatam, 1, 4))
  df$CIE10_4 <- paste0(letra, substr(df$codigo_redatam, 3, 4))
  df$CIE10   <- paste0(letra, substr(df$codigo_redatam, 3, 5))
  df[, c("codigo_redatam", "INEC_4", "CIE10_4", "CIE10", "nombre_cie10")]
}

diccionario <- bind_rows(lapply(seq_len(nrow(listas)), function(i) {
  ruta <- file.path(carpeta_excel, listas$archivo[i])
  if (!file.exists(ruta)) stop("No existe: ", ruta)

  message("Leyendo: ", listas$archivo[i])
  raw <- leer_lista(ruta)
  message("  códigos: ", nrow(raw), " | primero: ", raw$codigo_redatam[1],
          " | último: ", raw$codigo_redatam[nrow(raw)])

  anios <- listas$anio_min[i]:listas$anio_max[i]
  bind_rows(lapply(anios, function(y) {
    raw$anio <- y
    raw
  }))
}))

# ---- Verificaciones ---------------------------------------------------------
cat("\nCódigos por año:\n")
print(count(diccionario, anio))
# Esperado: 2015-2016 = 12421 | 2017-2020 = 12525 | 2021-2025 = 12530

falta_01000 <- setdiff(unique(diccionario$anio),
                       diccionario$anio[diccionario$codigo_redatam == "01000"])
if (length(falta_01000) > 0) stop("Falta 01000 en: ", paste(falta_01000, collapse = ", "))

dups <- diccionario |> count(anio, codigo_redatam) |> filter(n > 1)
if (nrow(dups) > 0) { print(dups); stop("Hay códigos duplicados dentro de un año.") }

cat("OK: 01000 presente en todos los años y sin duplicados.\n")

mezcla <- diccionario |> group_by(anio, INEC_4) |>
  summarise(n_cat = n_distinct(CIE10_4), .groups = "drop") |> filter(n_cat > 1)
if (nrow(mezcla) > 0) { print(mezcla); stop("Algún INEC_4 corresponde a más de una CIE10_4.") }
cat("OK: cada INEC_4 corresponde a una sola CIE10_4.\n")
# ---- Guardar ----------------------------------------------------------------
write_csv(
  diccionario,
  file.path(carpeta_datos, "diccionario_inec_cie10_por_anio.csv")
)

cat("Filas:", nrow(diccionario), "\n")
print(head(diccionario, 10))