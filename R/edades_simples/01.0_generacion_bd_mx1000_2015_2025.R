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
#   sexo        Hombre, Mujer y Total
#   anio        año
#   region      provincia de residencia
#   exposicion  población (estimacion de personas vivas en cada anio)
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
# =============================================================================

library(tidyverse)
library(readxl)
library(stringr)

source(here::here("R", "00.0_config.R"))

# -----------------------------------------------------------------------------
# Configuración (tomada de 00.0_config.R)
# -----------------------------------------------------------------------------
# archivo_poblacion, patron_defunciones_edad_simple, orden_provincias y
# orden_sexos vienen de la config.

# En la hoja C10 del INEC cada sexo ocupa un rango de columnas.
columnas_sexo_c10 <- list(
  Total  = 2:93,
  Hombre = 95:186,
  Mujer  = 188:279
)


# -----------------------------------------------------------------------------
# Validación de entrada
# -----------------------------------------------------------------------------

# Excel de defunciones, uno por año (se leen en el PASO 2).
archivos_defunciones <- list.files(
  path       = dir_raw,
  pattern    = patron_defunciones_edad_simple,
  full.names = TRUE
) %>%
  sort()

validar_archivos(
  archivo_poblacion,
  ayuda = "Ponga los Excel del INEC en data-raw/ o defina CR_MORT_DATA_RAW."
)

if (length(archivos_defunciones) == 0) {
  stop("No hay archivos Defunciones_edadessimples_YYYY.xlsx en ", dir_raw,
       "\nPonga los Excel del INEC en data-raw/ o defina CR_MORT_DATA_RAW.", call. = FALSE)
}

hojas_poblacion <- excel_sheets(archivo_poblacion)
if (!"C10" %in% hojas_poblacion) {
  stop(basename(archivo_poblacion), " no tiene la hoja C10. Hojas encontradas: ",
       paste(hojas_poblacion, collapse = ", "), call. = FALSE)
}


# =============================================================================
# PASO 1. Leer la población 
# =============================================================================
# La hoja C10 no es una tabla normal.
# Cada año es un bloque: una fila con el año, una con las edades
# y siete filas con las provincias.
# Cada uno de sus bloques de informacion contiene formulas de Excel.
# Este paso convierte eso en una fila por edad, sexo, año y provincia.

datos_pob <- read_excel(
  path          = archivo_poblacion,
  sheet         = "C10",
  col_names     = FALSE,
  .name_repair  = "minimal"
)

if (ncol(datos_pob) < max(unlist(columnas_sexo_c10))) {
  stop("La hoja C10 tiene ", ncol(datos_pob), " columnas; se esperaban al menos ",
       max(unlist(columnas_sexo_c10)), " (bloques Total, Hombre y Mujer).", call. = FALSE)
}

# Filas que contienen un año (2000 a 2050).
filas_con_anio <- which(
  apply(datos_pob, 1, function(fila) {
    any(str_detect(as.character(fila), "^20[0-5][0-9]$"))
  })
)

if (length(filas_con_anio) == 0) {
  stop("No se encontró ninguna fila con años (2000-2050) en la hoja C10.", call. = FALSE)
}

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

poblacion_c10 <- extraer_poblacion_c10()

# En los años de estudio, las provincias deben tener los nombres del INEC;
# si no, la población no se pega con las defunciones.
validar_valores(
  poblacion_c10$region[poblacion_c10$anio %in% anios_estudio],
  orden_provincias, "provincia", "Hoja C10 de población"
)

poblacion <- poblacion_c10 %>%
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
#   - la edad Ignorada se conserva

extraer_un_anio <- function(ruta_excel) {

  df <- read_excel(ruta_excel, col_names = FALSE, col_types = "text")

  if (ncol(df) < 10) {
    stop(basename(ruta_excel), " tiene ", ncol(df), " columnas; se esperaban al menos 10 ",
         "(edad en la columna 3 y las 7 provincias de la 4 a la 10).", call. = FALSE)
  }

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

mortalidad_excel <- map_dfr(archivos_defunciones, extraer_un_anio)

validar_anios(mortalidad_excel$anio, "Excel de defunciones por edad simple")

# Cada año debe venir de un solo Excel.
anios_repetidos <- mortalidad_excel %>%
  count(anio, sexo, region, edad) %>%
  filter(n > 1) %>%
  distinct(anio)
if (nrow(anios_repetidos) > 0) {
  stop("Hay más de un Excel de defunciones con el año: ",
       paste(anios_repetidos$anio, collapse = ", "),
       "\nRevise el año de la fila 6 de cada archivo.", call. = FALSE)
}

# Cada año debe traer los tres bloques: Hombres, Mujeres y Total.
bloques_faltantes <- expand_grid(anio = unique(mortalidad_excel$anio), sexo = orden_sexos) %>%
  anti_join(distinct(mortalidad_excel, anio, sexo), by = c("anio", "sexo"))
if (nrow(bloques_faltantes) > 0) {
  stop("Faltan bloques de sexo en los Excel de defunciones: ",
       paste(bloques_faltantes$anio, bloques_faltantes$sexo, collapse = "; "),
       "\nLa columna 2 de cada Excel debe decir Hombres, Mujeres y Total.", call. = FALSE)
}

# La hoja C10 debe tener población para todos los años con defunciones.
anios_sin_poblacion <- setdiff(unique(mortalidad_excel$anio), anios_poblacion)
if (length(anios_sin_poblacion) > 0) {
  stop("La hoja C10 no tiene población para: ",
       paste(anios_sin_poblacion, collapse = ", "), call. = FALSE)
}

mortalidad <- mortalidad_excel %>%
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
# mx      = muertes / población          
# mx_1000 = muertes / población × 1 000  
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
# PASO 4. Guardar (nombres unificados desde la config)
# =============================================================================

# Guardar por año
walk(sort(unique(base_mortalidad$anio)), function(anio_i) {
  write_csv(
    filter(base_mortalidad, anio == anio_i),
    salida$edad_simple_anio(anio_i)
  )
})

# Guardar base consolidada
write_csv(base_mortalidad, salida$edad_simple)

cat("\nGuardado:\n")
cat("  ", salida$edad_simple, "\n")
cat("  Archivos por año en", dir_procesados, "\n")