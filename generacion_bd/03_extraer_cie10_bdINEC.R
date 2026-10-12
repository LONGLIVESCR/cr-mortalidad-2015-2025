# =============================================================================
# Defunciones por causa (CIE-10), provincia, sexo y edad
# Costa Rica, 2015-2025
# =============================================================================
#
# Entrada
#   Defunciones_YYYY.xlsx en la carpeta de OneDrive.
#   Misma estructura INEC: bloques Hombres / Mujeres por provincia,
#   filas = código CIE, columnas = grupos de edad.
#
# Salida
#   Un solo CSV, todos los años:
#   mortalidad_cie10_2015_2025.csv
#
# Columnas
#   edad       0 | 1-4 | 5-9 | ... | 85-89 | 90 años y más | Edad ignorada | Total
#   sexo       Hombre | Mujer
#   anio       año de registro
#   region     provincia de residencia (sin el total nacional)
#   CIE10_4    código de causa
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

carpeta <- "dirección de la carpeta"

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
  "Edad ignorada",
  "Total"
)

edades_originales <- c(
  "Menores de 1 año", "1-4 años", "5-9 años", "10-14 años", "15-19 años",
  "20-24 años", "25-29 años", "30-34 años", "35-39 años", "40-44 años",
  "45-49 años", "50-54 años", "55-59 años", "60-64 años", "65-69 años",
  "70-74 años", "75-79 años", "80-84 años", "85-89 años", "90-94 años",
  "95-99 años", "100 y más años", "Edad ignorada", "Total"
)

provincias <- c(
  "1" = "San José",
  "2" = "Alajuela",
  "3" = "Cartago",
  "4" = "Heredia",
  "5" = "Guanacaste",
  "6" = "Puntarenas",
  "7" = "Limón"
)


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
      region <- provincias[area_num]

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

    names(bloque)[3] <- "CIE10_4"
    names(bloque)[4:27] <- edades_originales

    bloque_long <- bloque %>%
      select(CIE10_4, all_of(edades_originales)) %>%
      mutate(across(-CIE10_4, ~ if_else(.x %in% c("-", "", NA), 0L, as.integer(.x)))) %>%
      pivot_longer(-CIE10_4, names_to = "edad", values_to = "mortalidad") %>%
      mutate(
        region  = region,
        anio    = anio,
        sexo    = sexo,
        CIE10_4 = str_trim(CIE10_4)
      ) %>%
      select(region, anio, sexo, edad, CIE10_4, mortalidad)

    resultados[[length(resultados) + 1]] <- bloque_long
  }

  bind_rows(resultados)
}


# -----------------------------------------------------------------------------
# Todos los años, una sola tabla
# -----------------------------------------------------------------------------

archivos <- list.files(
  carpeta,
  pattern = "^Defunciones_\\d{4}\\.xlsx$",
  full.names = TRUE
) %>%
  sort()

message("Archivos encontrados: ", length(archivos))
print(basename(archivos))

defunciones <- map_dfr(archivos, extraer_defunciones)

causas_validas <- defunciones %>%
  filter(region != "Costa Rica") %>%
  distinct(region, anio, CIE10_4)

base_final <- defunciones %>%
  filter(region != "Costa Rica") %>%
  mutate(edad = armonizar_edad(edad)) %>%
  group_by(region, anio, sexo, edad, CIE10_4) %>%
  summarise(mortalidad = sum(mortalidad, na.rm = TRUE), .groups = "drop") %>%
  complete(
    region, anio, CIE10_4, edad, sexo,
    fill = list(mortalidad = 0)
  ) %>%
  semi_join(causas_validas, by = c("region", "anio", "CIE10_4")) %>%
  mutate(
    region = factor(region, levels = orden_regiones),
    sexo   = factor(sexo, levels = c("Hombre", "Mujer")),
    edad   = factor(edad, levels = orden_edades)
  ) %>%
  arrange(anio, region, as.integer(CIE10_4), edad, sexo) %>%
  select(edad, sexo, anio, region, CIE10_4, mortalidad)

write_csv(
  base_final,
  file.path(carpeta, "mortalidad_cie10_2015_2025.csv")
)

cat("Filas:", nrow(base_final), "\n")
cat("Años:", paste(sort(unique(base_final$anio)), collapse = ", "), "\n")
cat("Edades:\n")
print(levels(base_final$edad))
print(head(base_final, 12))
