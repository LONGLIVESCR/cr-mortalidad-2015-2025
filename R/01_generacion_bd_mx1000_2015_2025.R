# =============================================================================
# Costa Rica, 2015-2025
# Construcción de la base de mortalidad por edad, sexo, año y provincia
# =============================================================================
#
# Qué produce este script
# -----------------------
# Una tabla lista para análisis, con una fila por combinación de:
#
#   edad        edad en años cumplidos
#   sexo        Hombre, Mujer o Total
#   anio        año
#   region      provincia de residencia
#   exposicion  población (personas)
#   mortalidad  número de defunciones
#   mx          mortalidad / exposicion
#   mx_1000     la misma tasa por 1 000 habitantes
#
# De dónde salen los datos
# ------------------------
# 1. Población: Excel INEC, hoja C10
#    (estimaciones por edad, sexo y provincia).
#
# 2. Defunciones: un Excel por año
#    Defunciones_edadessimples_2015.xlsx ... 2025.xlsx
#    (sexo × edad × provincia).
#
# Cómo leer este archivo 
# -------------------------------------
# Cada bloque grande es un paso.
# Primero se declara la carpeta y las reglas.
# Después se lee la población.
# Después se leen las muertes.
# Después se unen y se calcula la tasa.
# Al final se guardan los CSV.
#
# =============================================================================

library(tidyverse)
library(readxl)
library(stringr)

options(scipen = 999)


# -----------------------------------------------------------------------------
# Configuración
# Cambie solo esta carpeta si los Excel están en otro lugar.
# -----------------------------------------------------------------------------

carpeta <- "C:dirección de la carpeta con todas las tablas excel"

archivo_poblacion <- file.path(
  carpeta,
  "poblacion_costarica_inec_2000-2050..xlsx"
)

orden_provincias <- c(
  "San José", "Alajuela", "Cartago", "Heredia",
  "Guanacaste", "Puntarenas", "Limón"
)

# En la hoja C10 del INEC cada sexo ocupa un rango de columnas.
columnas_sexo_c10 <- list(
  Total  = 2:93,
  Hombre = 95:186,
  Mujer  = 188:279
)


# =============================================================================
# PASO 1. Leer la población 
# =============================================================================
# La hoja C10 no viene como una tabla normal.
# Cada año es un bloque: una fila con el año, una con las edades
# y siete filas con las provincias.
# Este paso convierte eso en una fila por edad, sexo, año y provincia.

datos_pob <- read_excel(
  path          = archivo_poblacion,
  sheet         = "C10",
  col_names     = FALSE,
  .name_repair  = "minimal"
)

# Filas que contienen un año (2000 a 2050).
filas_con_anio <- which(
  apply(datos_pob, 1, function(fila) {
    any(str_detect(as.character(fila), "^20[0-5][0-9]$"))
  })
)

anios_poblacion <- as.integer(as.character(datos_pob[[3]][filas_con_anio]))

# Las etiquetas de edad están justo debajo del primer año.
fila_etiquetas_edad <- filas_con_anio[1] + 1

edades_poblacion <- as.character(
  unlist(datos_pob[fila_etiquetas_edad, columnas_sexo_c10$Total])
)
edades_poblacion[is.na(edades_poblacion) | edades_poblacion %in% c("", "NA")] <- "Total"
edades_poblacion <- str_trim(edades_poblacion)

# C10 cierra las edades altas en un solo grupo.
edades_poblacion <- if_else(
  str_detect(edades_poblacion, "90"),
  "90 años y más",
  edades_poblacion
)

extraer_poblacion_c10 <- function() {

  lista <- list()

  for (i in seq_along(filas_con_anio)) {

    fila_anio     <- filas_con_anio[i]
    anio          <- anios_poblacion[i]
    filas_prov    <- (fila_anio + 2):(fila_anio + 8)
    provincias    <- str_trim(as.character(datos_pob[[1]][filas_prov]))

    for (sexo in names(columnas_sexo_c10)) {

      columnas <- columnas_sexo_c10[[sexo]]

      for (j in seq_along(filas_prov)) {

        valores <- suppressWarnings(
          as.numeric(unlist(datos_pob[filas_prov[j], columnas]))
        )

        lista[[length(lista) + 1]] <- tibble(
          edad       = edades_poblacion,
          sexo       = sexo,
          anio       = anio,
          region     = provincias[j],
          exposicion = as.integer(round(valores, 0))
        )
      }
    }
  }

  bind_rows(lista)
}

poblacion <- extraer_poblacion_c10() %>%
  filter(edad != "Total") %>%
  mutate(
    region = factor(region, levels = orden_provincias),
    sexo   = factor(sexo,   levels = c("Hombre", "Mujer", "Total"))
  ) %>%
  select(edad, sexo, anio, region, exposicion)


# =============================================================================
# PASO 2. Leer las defunciones 
# =============================================================================
# Cada Excel anual tiene tres bloques: Hombres, Mujeres y Total.
# En cada bloque hay una fila por edad y una columna por provincia.
#
# Reglas de limpieza:
#   - el guion del INEC significa cero muertes
#   - las edades 90, 91, 92... se suman en "90 años y más"
#   - se quita la fila Total de edad (es solo la suma)
#   - no se usa la columna nacional Total
#   - la edad Ignorada se conserva

extraer_un_anio <- function(ruta_excel) {

  df <- read_excel(ruta_excel, col_names = FALSE, col_types = "text")

  anio <- as.integer(str_extract(as.character(df[[1]][6]), "\\d{4}"))
  if (is.na(anio)) {
    anio <- as.integer(str_extract(basename(ruta_excel), "\\d{4}"))
  }

  col_sexo <- df[[2]]

  inicios <- tibble(
    fila = c(
      which(col_sexo == "Hombres")[1],
      which(col_sexo == "Mujeres")[1],
      which(col_sexo == "Total")[1]
    ),
    sexo = c("Hombre", "Mujer", "Total")
  ) %>%
    filter(!is.na(fila)) %>%
    arrange(fila)

  extraer_bloque <- function(i) {

    fila_inicio <- inicios$fila[i]
    sexo        <- inicios$sexo[i]

    fila_fin <- if (i < nrow(inicios)) {
      inicios$fila[i + 1] - 1
    } else {
      notas <- which(str_detect(
        df[[1]],
        regex("Instituto Nacional|Datos preliminares|Estimada población",
              ignore_case = TRUE)
      ))
      if (length(notas) > 0) min(notas) - 1 else nrow(df)
    }

    df[(fila_inicio + 1):fila_fin, ] %>%
      filter(!is.na(.[[3]])) %>%
      transmute(
        edad_original = str_trim(as.character(.[[3]])),
        `San José`    = .[[4]],
        Alajuela      = .[[5]],
        Cartago       = .[[6]],
        Heredia       = .[[7]],
        Guanacaste    = .[[8]],
        Puntarenas    = .[[9]],
        Limón         = .[[10]]
      ) %>%
      filter(!str_detect(edad_original, regex("^Total$", ignore_case = TRUE))) %>%
      pivot_longer(
        cols      = all_of(orden_provincias),
        names_to  = "region",
        values_to = "mortalidad"
      ) %>%
      mutate(
        mortalidad = as.integer(
          if_else(mortalidad %in% c("-", "", NA), "0", mortalidad)
        ),
        edad = case_when(
          str_detect(edad_original, regex("ignor", ignore_case = TRUE)) ~ "Ignorada",
          suppressWarnings(as.integer(edad_original)) >= 90            ~ "90 años y más",
          TRUE ~ edad_original
        ),
        sexo = sexo,
        anio = anio
      ) %>%
      group_by(edad, sexo, anio, region) %>%
      summarise(mortalidad = sum(mortalidad, na.rm = TRUE), .groups = "drop")
  }

  map_dfr(seq_len(nrow(inicios)), extraer_bloque)
}

archivos_defunciones <- list.files(
  path       = carpeta,
  pattern    = "^Defunciones_edadessimples_\\d{4}\\.xlsx$",
  full.names = TRUE
) %>%
  sort()

mortalidad <- map_dfr(archivos_defunciones, extraer_un_anio) %>%
  mutate(
    region = factor(region, levels = orden_provincias),
    sexo   = factor(sexo,   levels = c("Hombre", "Mujer", "Total"))
  ) %>%
  select(edad, sexo, anio, region, mortalidad)


# =============================================================================
# PASO 3. Unir población y defunciones, y calcular la tasa
# =============================================================================
# Se pegan las dos tablas por las mismas cuatro llaves:
# edad + sexo + anio + region.
#
# mx      = muertes / población          (para tablas de mortalidad)
# mx_1000 = muertes / población × 1 000  (para describir el nivel)
#
# Si no hay población (casi siempre edad Ignorada), la tasa queda vacía.

base_mortalidad <- mortalidad %>%
  left_join(
    poblacion,
    by = c("edad", "sexo", "anio", "region")
  ) %>%
  mutate(
    mx = if_else(
      is.na(exposicion) | exposicion == 0,
      NA_real_,
      mortalidad / exposicion
    ),
    mx_1000  = mx * 1000,
    edad_num = suppressWarnings(as.integer(
      if_else(edad == "90 años y más", "90", edad)
    ))
  ) %>%
  arrange(anio, region, sexo, is.na(edad_num), edad_num) %>%
  select(edad, sexo, anio, region, exposicion, mortalidad, mx, mx_1000)


# =============================================================================
# PASO 4. Guardar
# =============================================================================

walk(sort(unique(base_mortalidad$anio)), function(anio_i) {
  write_csv(
    filter(base_mortalidad, anio == anio_i),
    file.path(carpeta, paste0("CR_base_mortalidad_", anio_i, ".csv"))
  )
})

write_csv(
  base_mortalidad,
  file.path(carpeta, "CR_base_mortalidad_2015_2025.csv")
)


# =============================================================================
# Comprobación breve
# =============================================================================

cat("Archivos de defunciones leídos:", length(archivos_defunciones), "\n")
cat("Filas de la base final:", nrow(base_mortalidad), "\n")
cat("Años:", paste(sort(unique(base_mortalidad$anio)), collapse = ", "), "\n")
cat("Filas sin exposición:", sum(is.na(base_mortalidad$exposicion)), "\n")
print(head(base_mortalidad, 12))

