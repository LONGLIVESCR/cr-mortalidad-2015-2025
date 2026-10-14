# =============================================================================
# Defunciones por causa (causa4dig), provincia, sexo y edad
# Costa Rica, 2015-2025
# =============================================================================
#
#   Defunciones_YYYY.xlsx
#   Misma estructura INEC: bloques Hombres / Mujeres por provincia,
#   filas = código de causa, columnas = grupos de edad quinquenal y menores a 1 anio.
#
# Salida (dir_procesados)
#   Un solo CSV, todos los años:
#   mortalidad_cie10_2015_2025.csv
#
# Columnas
#   edad       0 | 1-4 | 5-9 | ... | 85-89 | 90 años y más | Edad ignorada | Total
#   sexo       Hombre, Mujer y Total
#   anio       año de registro
#   region     provincia de residencia
#   INEC_4     código numérico REDATAM causa4dig, como entero (104, 921, 1044...).
#              NO es todavía el código CIE-10
#   mortalidad defunciones
#
# Edades
#   Menores de 1 año              -> 0
#   1-4 años                      -> 1-4
#   5-9 ... 85-89                 -> se quita la palabra "años"
#   90-94 + 95-99 + 100 y más     -> 90 años y más
#   Edad ignorada y Total         -> se conservan
#
# Si un sexo no tiene muertes en esa causa, se escribe 0.
# No se une población.
# =============================================================================

library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(stringr)
library(purrr)

source("R/00.0_config.R")

orden_edades <- c(grupos_edad_quinquenal, "Edad ignorada", "Total")


# -----------------------------------------------------------------------------
# Edad final
# -----------------------------------------------------------------------------

armonizar_edad <- function(edad) {
  case_when(
    edad == "Menores de 1 año" ~ "0",
    edad %in% c("90-94 años", "95-99 años", "100 y más años") ~ "90 años y más",
    edad == "Edad ignorada" ~ "Edad ignorada",
    edad == "Total" ~ "Total",
    TRUE ~ str_remove(edad, " años")
  )
}


# -----------------------------------------------------------------------------
# Un Excel anual
# -----------------------------------------------------------------------------

extraer_defunciones <- function(ruta_archivo) {

  message("→ ", basename(ruta_archivo))

  df <- read_excel(ruta_archivo, col_names = FALSE, col_types = "text")

  anio <- as.integer(str_extract(df[[1]][6], "\\d{4}"))
  if (is.na(anio)) {
    anio <- as.integer(str_extract(basename(ruta_archivo), "\\d{4}"))
  }

  col2 <- df[[2]]
  area_idx    <- which(str_detect(col2, "^AREA #"))
  hombres_idx <- which(col2 == "Hombres")
  mujeres_idx <- which(col2 == "Mujeres")
  total_idx   <- which(col2 == "Total")
  sex_starts  <- sort(c(hombres_idx, mujeres_idx))

  resultados <- list()

  for (s in seq_along(sex_starts)) {

    start_row <- sex_starts[s]
    sexo <- if (col2[start_row] == "Hombres") "Hombre" else "Mujer"

    areas_antes <- area_idx[area_idx < start_row]
    if (length(areas_antes) == 0) {
      region <- "Costa Rica"
    } else {
      last_area_row <- max(areas_antes)
      area_num <- as.character(df[[3]][last_area_row])
      region <- codigos_provincia[area_num]

      if (!is.na(area_num) && as.integer(area_num) == 7) {
        if (any(total_idx > last_area_row & total_idx < start_row)) {
          region <- "Costa Rica"
        }
      }
    }

    if (is.na(region)) next

    candidatos_fin <- c(sex_starts[sex_starts > start_row], total_idx[total_idx > start_row])
    end_row <- if (length(candidatos_fin) == 0) {
      nrow(df)
    } else {
      min(candidatos_fin) - 1
    }

    bloque <- df[(start_row + 1):end_row, ]
    bloque <- bloque[!is.na(bloque[[3]]) & str_detect(bloque[[3]], "^\\s*[0-9]+\\s*$"), ]

    if (nrow(bloque) == 0) next

    names(bloque)[3] <- "INEC_4"
    names(bloque)[4:27] <- edades_originales_causa

    bloque_long <- bloque %>%
      select(INEC_4, all_of(edades_originales_causa)) %>%
      mutate(across(-INEC_4, ~ if_else(.x %in% c("-", "", NA), 0L, as.integer(.x)))) %>%
      pivot_longer(-INEC_4, names_to = "edad", values_to = "mortalidad") %>%
      mutate(
        region = unname(region),
        anio   = anio,
        sexo   = sexo,
        INEC_4 = str_trim(INEC_4)
      ) %>%
      select(region, anio, sexo, edad, INEC_4, mortalidad)

    resultados[[length(resultados) + 1]] <- bloque_long
  }

  bind_rows(resultados)
}


# -----------------------------------------------------------------------------
# Todos los años, una sola tabla
# -----------------------------------------------------------------------------

archivos <- list.files(
  dir_raw,
  pattern = patron_defunciones_causa,
  full.names = TRUE
) %>%
  sort()

message("Archivos encontrados: ", length(archivos))
print(basename(archivos))
if (length(archivos) == 0) stop("No hay archivos Defunciones_YYYY.xlsx en ", dir_raw)

defunciones <- map_dfr(archivos, extraer_defunciones)

# causa4dig tiene como máximo 4 cifras. Se guarda como entero (0104 -> 104).
if (!all(str_detect(defunciones$INEC_4, "^[0-9]{1,4}$"))) {
  print(defunciones %>% filter(!str_detect(INEC_4, "^[0-9]{1,4}$")) %>% distinct(anio, INEC_4))
  stop("Hay códigos de causa que no son numéricos de 1 a 4 cifras.")
}
defunciones <- defunciones %>% mutate(INEC_4 = as.integer(INEC_4))

causas_validas <- defunciones %>%
  filter(region != "Costa Rica") %>%
  distinct(region, anio, INEC_4)

base_final <- defunciones %>%
  filter(region != "Costa Rica") %>%
  mutate(edad = armonizar_edad(edad)) %>%
  group_by(region, anio, sexo, edad, INEC_4) %>%
  summarise(mortalidad = sum(mortalidad, na.rm = TRUE), .groups = "drop") %>%
  complete(
    region, anio, INEC_4, edad, sexo,
    fill = list(mortalidad = 0)
  ) %>%
  semi_join(causas_validas, by = c("region", "anio", "INEC_4")) %>%
  mutate(
    region = factor(region, levels = orden_provincias),
    sexo   = factor(sexo, levels = c("Hombre", "Mujer")),
    edad   = factor(edad, levels = orden_edades)
  ) %>%
  arrange(anio, region, INEC_4, edad, sexo) %>%
  select(edad, sexo, anio, region, INEC_4, mortalidad)

if (anyNA(base_final$region) || anyNA(base_final$edad) || anyNA(base_final$sexo)) {
  stop("Hay provincias, edades o sexos fuera de los niveles esperados.")
}

write_csv(base_final, salida$cie10)

cat("Filas:", nrow(base_final), "\n")
cat("Años:", paste(sort(unique(base_final$anio)), collapse = ", "), "\n")
cat("Edades:\n")
print(levels(base_final$edad))
print(head(base_final, 12))
