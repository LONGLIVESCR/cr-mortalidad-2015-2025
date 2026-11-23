# ============================================================
# PROYECTO LONGLIVES_CR
# IDENTIFICACIÓN Y ANÁLISIS DE DEFUNCIONES TARDÍAS
# Fuente: Estadísticas vitales - INEC Costa Rica
# Archivos REDATAM: 2000-2025
# 52 archivos: 26 años x 2 sexos
# ============================================================


# ============================================================
# 1. INSTALAR Y CARGAR PAQUETES
# ============================================================

# Ejecutar install.packages() únicamente si falta algún paquete.

# install.packages(c(
#   "tidyverse",
#   "readxl",
#   "janitor",
#   "stringr",
#   "purrr",
#   "writexl"
# ))

library(tidyverse)
library(readxl)
library(janitor)
library(stringr)
library(purrr)
library(writexl)


# ============================================================
# 2. DEFINIR CARPETA DE TRABAJO
# ============================================================

ruta <- "/Users/jose/Desktop/UNA Proyecto/Registro_tardio"
# o la ruta exacta que te dé Finder

# Validar existencia de la carpeta.

if (!dir.exists(ruta)) {
  stop("No se encuentra la carpeta de trabajo: ", ruta)
}


# ============================================================
# 3. IDENTIFICAR LOS ARCHIVOS EXCEL
# ============================================================

archivos <- list.files(
  path = ruta,
  pattern = "\\.xlsx$",
  full.names = TRUE
)

# Mostrar cantidad de archivos.

cat("\nNúmero de archivos Excel encontrados:", length(archivos), "\n")

# Se esperan 52 archivos:
# 26 años (2000-2025) x 2 sexos.

if (length(archivos) != 52) {
  stop(
    "Se esperaban 52 archivos Excel, pero se encontraron ",
    length(archivos),
    ". Revisar la carpeta."
  )
}


# ============================================================
# 4. CONSTRUIR METADATOS DE LOS ARCHIVOS
# ============================================================

# Extraer el año del nombre del archivo.
# Leer VARIABLE.SEXO desde la celda A8.

meta_archivos <- tibble(
  archivo = archivos,
  nombre = basename(archivos)
) %>%
  mutate(
    universo = as.integer(
      str_extract(nombre, "^\\d{4}")
    )
  ) %>%
  mutate(
    sexo_codigo = map_chr(
      archivo,
      function(x) {
        
        celda <- read_excel(
          x,
          range = "A8",
          col_names = FALSE,
          .name_repair = "minimal"
        )
        
        as.character(celda[[1]][1])
      }
    )
  ) %>%
  mutate(
    sexo = case_when(
      str_detect(sexo_codigo, "VARIABLE\\.SEXO=1") ~ "Hombre",
      str_detect(sexo_codigo, "VARIABLE\\.SEXO=2") ~ "Mujer",
      TRUE ~ NA_character_
    )
  ) %>%
  arrange(universo, sexo)


# ------------------------------------------------------------
# 4.1. Inspeccionar metadatos
# ------------------------------------------------------------

print(meta_archivos, n = 60)


# ------------------------------------------------------------
# 4.2. Comprobar distribución por año y sexo
# ------------------------------------------------------------

distribucion_archivos <- meta_archivos %>%
  count(universo, sexo, name = "archivos") %>%
  arrange(universo, sexo)

print(distribucion_archivos, n = Inf)


# ------------------------------------------------------------
# 4.3. Identificar metadatos faltantes
# ------------------------------------------------------------

metadatos_faltantes <- meta_archivos %>%
  filter(
    is.na(universo) |
      is.na(sexo)
  )

print(metadatos_faltantes, n = Inf)


# ------------------------------------------------------------
# 4.4. Identificar combinaciones duplicadas
# ------------------------------------------------------------

duplicados_meta <- meta_archivos %>%
  count(
    universo,
    sexo,
    name = "frecuencia"
  ) %>%
  filter(frecuencia != 1)

print(duplicados_meta, n = Inf)


# ------------------------------------------------------------
# 4.5. Validaciones de metadatos
# ------------------------------------------------------------

if (any(is.na(meta_archivos$universo))) {
  stop("Hay archivos cuyo año no pudo identificarse.")
}

if (any(is.na(meta_archivos$sexo))) {
  stop("Hay archivos cuyo sexo no pudo identificarse.")
}

if (nrow(duplicados_meta) > 0) {
  stop("Hay combinaciones duplicadas de universo y sexo.")
}

if (n_distinct(meta_archivos$universo) != 26) {
  stop("No se identificaron los 26 años esperados.")
}

if (n_distinct(meta_archivos$sexo) != 2) {
  stop("No se identificaron los dos sexos esperados.")
}

if (nrow(meta_archivos) != 52) {
  stop("La tabla de metadatos no contiene 52 archivos.")
}

cat(
  "\nVALIDACIÓN DE METADATOS COMPLETADA.",
  "\nArchivos:", nrow(meta_archivos),
  "\nUniversos:", n_distinct(meta_archivos$universo),
  "\nSexos:", n_distinct(meta_archivos$sexo),
  "\n"
)


# ============================================================
# 5. DEFINIR CATEGORÍAS DE EDAD
# ============================================================

edades_esperadas <- c(
  "Menores de 1 año",
  "1-4 años",
  "5-9 años",
  "10-14 años",
  "15-19 años",
  "20-24 años",
  "25-29 años",
  "30-34 años",
  "35-39 años",
  "40-44 años",
  "45-49 años",
  "50-54 años",
  "55-59 años",
  "60-64 años",
  "65-69 años",
  "70-74 años",
  "75-79 años",
  "80-84 años",
  "85-89 años",
  "90-94 años",
  "95-99 años",
  "100 y más años",
  "Edad ignorada"
)

# Comprobar número de categorías.

if (length(edades_esperadas) != 23) {
  stop("La definición de categorías de edad no contiene 23 grupos.")
}

cat(
  "\nCategorías de edad definidas:",
  length(edades_esperadas),
  "\n"
)

print(edades_esperadas)

# ============================================================
# 6. FUNCIÓN PARA LEER Y PROCESAR ARCHIVOS REDATAM
# ============================================================

leer_redatam <- function(archivo, universo, sexo) {

  # ----------------------------------------------------------
  # 6.1. Leer el archivo y reconocer su formato
  # ----------------------------------------------------------

  datos <- read_excel(
    archivo,
    skip = 13,
    col_names = FALSE,
    col_types = "text",
    .name_repair = "minimal"
  )

  numero_columnas <- ncol(datos)
  names(datos) <- paste0("v", seq_len(numero_columnas))

  # Formato habitual: 23 edades + total (27 columnas).
  # Excepción 2012 mujeres: 22 edades + total (26 columnas).
  if (numero_columnas == 27) {
    columnas_edad <- paste0("v", 4:26)
    columna_total <- "v27"
  } else if (numero_columnas == 26) {
    columnas_edad <- paste0("v", 4:25)
    columna_total <- "v26"
  } else {
    stop(
      "Número inesperado de columnas en ", basename(archivo),
      ": ", numero_columnas
    )
  }

  # ----------------------------------------------------------
  # 6.2. Delimitar los datos provinciales
  # ----------------------------------------------------------

  # En los 52 archivos, RESUMEN es el total del universo.
  # Se excluye esa fila y absolutamente todo lo que sigue.
  fila_resumen <- which(
    str_detect(
      str_trim(coalesce(datos$v2, "")),
      regex("^RESUMEN$", ignore_case = TRUE)
    )
  )

  if (length(fila_resumen) != 1L) {
    stop(
      "Se esperaba exactamente una fila RESUMEN en ",
      basename(archivo), "; se encontraron ", length(fila_resumen), "."
    )
  }

  datos <- datos %>%
    slice(seq_len(fila_resumen - 1L))

  # ----------------------------------------------------------
  # 6.3. Identificar las siete áreas provinciales
  # ----------------------------------------------------------

  datos <- datos %>%
    mutate(
      provincia_id = if_else(
        str_detect(
          str_trim(coalesce(v2, "")),
          regex("^AREA\\s*#\\s*[0-9]+$", ignore_case = TRUE)
        ),
        suppressWarnings(as.integer(str_extract(v3, "[0-9]+"))),
        NA_integer_
      )
    ) %>%
    fill(provincia_id, .direction = "down")

  provincias_en_archivo <- sort(unique(na.omit(datos$provincia_id)))

  if (!identical(provincias_en_archivo, 1:7)) {
    stop(
      "La estructura provincial no corresponde a las siete provincias en ",
      basename(archivo), ". Códigos encontrados: ",
      paste(provincias_en_archivo, collapse = ", ")
    )
  }

  # ----------------------------------------------------------
  # 6.4. Identificar encabezados de edad y bloques Total
  # ----------------------------------------------------------

  datos <- datos %>%
    mutate(
      es_encabezado_edades = str_detect(
        str_trim(coalesce(v4, "")),
        regex("^Menores de 1 año$", ignore_case = TRUE)
      ),
      es_total_acumulado = es_encabezado_edades &
        str_detect(
          str_trim(coalesce(v2, "")),
          regex("^Total$", ignore_case = TRUE)
        )
    ) %>%
    group_by(provincia_id) %>%
    mutate(
      despues_total_acumulado = cumsum(
        as.integer(es_total_acumulado)
      ) > 0L
    ) %>%
    ungroup() %>%
    filter(!despues_total_acumulado)

  # ----------------------------------------------------------
  # 6.5. Recuperar los años de declaración y defunción
  # ----------------------------------------------------------

  datos <- datos %>%
    mutate(
      anio_declaracion = if_else(
        es_encabezado_edades &
          str_detect(str_trim(coalesce(v2, "")), "^[0-9]{4}$"),
        suppressWarnings(as.integer(str_trim(v2))),
        NA_integer_
      )
    ) %>%
    group_by(provincia_id) %>%
    fill(anio_declaracion, .direction = "down") %>%
    ungroup() %>%
    mutate(
      anio_defuncion = if_else(
        str_detect(str_trim(coalesce(v3, "")), "^[0-9]{4}$"),
        suppressWarnings(as.integer(str_trim(v3))),
        NA_integer_
      )
    ) %>%
    filter(!is.na(anio_defuncion))

  if (nrow(datos) == 0L) {
    stop("No se identificaron filas de años de defunción en ", basename(archivo))
  }

  if (anyNA(datos$anio_declaracion)) {
    stop("Hay filas de defunción sin año de declaración en ", basename(archivo))
  }

  
  # ----------------------------------------------------------
  # 6.6. Nombrar y armonizar las columnas de edad
  # ----------------------------------------------------------
  
  # Identificar si el archivo corresponde a la excepción
  # documentada de 2012 mujeres.
  
  es_2012_mujeres <- (
    universo == 2012L &&
      sexo == "Mujer"
  )
  
  
  # Validar estrictamente el número de columnas esperado.
  
  if (es_2012_mujeres && numero_columnas != 26L) {
    
    stop(
      "El archivo de mujeres de 2012 debe tener 26 columnas: ",
      basename(archivo)
    )
    
  }
  
  if (!es_2012_mujeres && numero_columnas != 27L) {
    
    stop(
      "El archivo no corresponde a la estructura habitual de 27 columnas: ",
      basename(archivo)
    )
    
  }
  
  
  # Renombrar las columnas originales de edad.
  
  names(datos)[match(columnas_edad, names(datos))] <-
    paste0("edad_", seq_along(columnas_edad))
  
  
  # Renombrar la columna de total original.
  
  names(datos)[match(columna_total, names(datos))] <-
    "total_redatam"
  
  
  # Incorporar la categoría Edad ignorada.
  #
  # En mujeres de 2012 esta categoría no existe en el archivo.
  # Se representa mediante NA, no mediante cero.
  #
  # En los demás archivos la categoría está disponible.
  
  if (es_2012_mujeres) {
    
    datos$edad_23 <- NA_character_
    
  }
  
  
  # Registrar la disponibilidad de la categoría.
  
  edad_ignorada_disponible <- !es_2012_mujeres
  
  
  # Definir las 23 columnas armonizadas.
  
  columnas_edad_finales <- paste0(
    "edad_",
    seq_along(edades_esperadas)
  )
  
  
  # Definir los nombres de las columnas que conservarán
  # los valores originales.
  
  columnas_valor_original <- paste0(
    "original_",
    columnas_edad_finales
  )
  
  
  # REDATAM representa cero con guion.
  # Se conserva el valor original antes de convertirlo.
  
  
  # Función para convertir los recuentos a valores numéricos.
  
  convertir_conteo <- function(x) {
    
    x <- str_trim(as.character(x))
    
    case_when(
      
      is.na(x) | x == "" ~ NA_real_,
      
      x == "-" ~ 0,
      
      TRUE ~ suppressWarnings(
        as.numeric(
          str_remove_all(x, ",")
        )
      )
    )
  }
  
  
  # Conservar los valores originales de las categorías de edad
  # antes de realizar la conversión numérica.
  
  datos <- datos %>%
    mutate(
      across(
        all_of(columnas_edad_finales),
        as.character,
        .names = "original_{.col}"
      ),
      
      across(
        all_of(columnas_edad_finales),
        convertir_conteo
      )
    )
  
  
  # ----------------------------------------------------------
  # 6.7. Pasar a formato largo e incorporar metadatos
  # ----------------------------------------------------------
  
  provincias <- tibble(
    provincia_id = 1:7,
    
    provincia = c(
      "San José",
      "Alajuela",
      "Cartago",
      "Heredia",
      "Guanacaste",
      "Puntarenas",
      "Limón"
    )
  )
  
  
  datos_largos <- datos %>%
    select(
      provincia_id,
      anio_declaracion,
      anio_defuncion,
      all_of(columnas_edad_finales),
      all_of(columnas_valor_original)
    ) %>%
    
    pivot_longer(
      cols = c(
        all_of(columnas_edad_finales),
        all_of(columnas_valor_original)
      ),
      names_to = c(".value", "categoria_id"),
      names_pattern = "(edad|original_edad)_(\\d+)"
    ) %>%
    
    rename(
      defunciones = edad,
      valor_original = original_edad
    ) %>%
    
    mutate(
      categoria_id = as.integer(categoria_id),
      
      edad_quinquenal = edades_esperadas[categoria_id],
      
      categoria_disponible = if_else(
        categoria_id == 23L,
        edad_ignorada_disponible,
        TRUE
      ),
      
      estado_registro = case_when(
        
        !categoria_disponible ~ "no_disponible",
        
        is.na(valor_original) |
          str_trim(valor_original) == "" ~ "celda_vacia",
        
        str_trim(valor_original) == "-" ~ "guion",
        
        !is.na(defunciones) & defunciones == 0 ~ "cero_explicito",
        
        !is.na(defunciones) & defunciones > 0 ~ "positivo",
        
        !is.na(defunciones) & defunciones < 0 ~ "negativo",
        
        TRUE ~ "otro_valor"
      )
    ) %>%
    
    left_join(
      provincias,
      by = "provincia_id"
    ) %>%
    
    mutate(
      universo = as.integer(universo),
      sexo = as.character(sexo)
    ) %>%
    
    select(
      universo,
      sexo,
      provincia_id,
      provincia,
      anio_declaracion,
      anio_defuncion,
      edad_quinquenal,
      categoria_disponible,
      valor_original,
      estado_registro,
      defunciones
    )
  
  
  # ----------------------------------------------------------
  # Validaciones internas de la salida
  # ----------------------------------------------------------
  
  
  # No se permiten valores faltantes en categorías disponibles.
  
  if (any(
    is.na(datos_largos$defunciones) &
    datos_largos$categoria_disponible
  )) {
    
    stop(
      "Hay recuentos faltantes en categorías disponibles en ",
      basename(archivo)
    )
    
  }
  
  
  # No se permiten valores originales no reconocidos.
  
  if (any(
    datos_largos$estado_registro == "otro_valor",
    na.rm = TRUE
  )) {
    
    stop(
      "Hay valores originales no reconocidos en ",
      basename(archivo)
    )
    
  }
  
  
  # No se permiten celdas vacías en categorías disponibles.
  
  if (any(
    datos_largos$estado_registro == "celda_vacia"
  )) {
    
    stop(
      "Hay celdas vacías en categorías disponibles en ",
      basename(archivo)
    )
    
  }
  
  
  # La categoría no disponible debe corresponder exclusivamente
  # a Edad ignorada en mujeres del universo 2012.
  
  if (any(
    !datos_largos$categoria_disponible &
    !(
      datos_largos$edad_quinquenal == "Edad ignorada" &
      datos_largos$universo == 2012L &
      datos_largos$sexo == "Mujer"
    )
  )) {
    
    stop(
      "Se detectó una categoría no disponible inesperada en ",
      basename(archivo)
    )
    
  }
  
  
  # Los guiones deben haberse convertido numéricamente en cero.
  
  if (any(
    datos_largos$defunciones[
      datos_largos$estado_registro == "guion"
    ] != 0,
    na.rm = TRUE
  )) {
    
    stop(
      "Hay guiones que no fueron convertidos en cero en ",
      basename(archivo)
    )
    
  }
  
  
  # No se permiten recuentos negativos.
  
  if (any(datos_largos$defunciones < 0, na.rm = TRUE)) {
    
    stop(
      "Hay conteos negativos en ",
      basename(archivo)
    )
    
  }
  
  
  # Comprobar las siete provincias y las 23 categorías armonizadas.
  
  if (
    n_distinct(datos_largos$provincia_id) != 7L ||
    n_distinct(datos_largos$edad_quinquenal) != 23L
  ) {
    
    stop(
      "La salida no contiene las siete provincias y las 23 ",
      "categorías armonizadas en ",
      basename(archivo)
    )
    
  }
  
  
  # Devolver la base procesada.
  
  datos_largos
  }

# ============================================================
# 6.17. PRUEBA CONTROLADA DE LA FUNCIÓN leer_redatam()
# ============================================================

# ------------------------------------------------------------
# 6.17.1. Identificar archivos de prueba
# ------------------------------------------------------------

archivo_2022_hombres <- meta_archivos %>%
  filter(
    universo == 2022,
    sexo == "Hombre"
  ) %>%
  pull(archivo)

archivo_2012_mujeres <- meta_archivos %>%
  filter(
    universo == 2012,
    sexo == "Mujer"
  ) %>%
  pull(archivo)


# ------------------------------------------------------------
# 6.17.2. Ejecutar lectura de los dos archivos
# ------------------------------------------------------------

prueba_2022_hombres <- leer_redatam(
  archivo = archivo_2022_hombres,
  universo = 2022,
  sexo = "Hombre"
)

prueba_2012_mujeres <- leer_redatam(
  archivo = archivo_2012_mujeres,
  universo = 2012,
  sexo = "Mujer"
)


# ------------------------------------------------------------
# 6.17.3. Examinar dimensiones
# ------------------------------------------------------------

cat("\nDimensiones 2022 hombres:\n")

print(dim(prueba_2022_hombres))


cat("\nDimensiones 2012 mujeres:\n")

print(dim(prueba_2012_mujeres))


# ------------------------------------------------------------
# 6.17.4. Examinar estructura de las bases
# ------------------------------------------------------------

cat("\nEstructura 2022 hombres:\n")

glimpse(prueba_2022_hombres)


cat("\nEstructura 2012 mujeres:\n")

glimpse(prueba_2012_mujeres)


# ------------------------------------------------------------
# 6.17.5. Examinar cobertura de provincias y edades
# ------------------------------------------------------------

cat("\nProvincias 2022 hombres:\n")

print(
  sort(unique(prueba_2022_hombres$provincia_id))
)


cat("\nProvincias 2012 mujeres:\n")

print(
  sort(unique(prueba_2012_mujeres$provincia_id))
)


cat("\nCategorías de edad 2022 hombres:\n")

print(
  n_distinct(prueba_2022_hombres$edad_quinquenal)
)


cat("\nCategorías de edad 2012 mujeres:\n")

print(
  n_distinct(prueba_2012_mujeres$edad_quinquenal)
)


# ------------------------------------------------------------
# 6.17.6. Examinar valores faltantes
# ------------------------------------------------------------

cat("\nValores faltantes 2022 hombres:\n")

print(
  colSums(is.na(prueba_2022_hombres))
)


cat("\nValores faltantes 2012 mujeres:\n")

print(
  colSums(is.na(prueba_2012_mujeres))
)


# ------------------------------------------------------------
# 6.17.7. Examinar valores negativos
# ------------------------------------------------------------

cat("\nDefunciones negativas 2022 hombres:\n")

print(
  sum(
    prueba_2022_hombres$defunciones < 0,
    na.rm = TRUE
  )
)


cat("\nDefunciones negativas 2012 mujeres:\n")

print(
  sum(
    prueba_2012_mujeres$defunciones < 0,
    na.rm = TRUE
  )
)


# ------------------------------------------------------------
# 6.17.8. Examinar años identificados
# ------------------------------------------------------------

cat("\nAños de declaración 2022 hombres:\n")

print(
  sort(unique(prueba_2022_hombres$anio_declaracion))
)


cat("\nAños de defunción 2022 hombres:\n")

print(
  sort(unique(prueba_2022_hombres$anio_defuncion))
)


cat("\nAños de declaración 2012 mujeres:\n")

print(
  sort(unique(prueba_2012_mujeres$anio_declaracion))
)


cat("\nAños de defunción 2012 mujeres:\n")

print(
  sort(unique(prueba_2012_mujeres$anio_defuncion))
)


# ------------------------------------------------------------
# 6.17.9. Examinar totales procesados
# ------------------------------------------------------------

cat("\nTotal de defunciones 2022 hombres:\n")

print(
  sum(
    prueba_2022_hombres$defunciones,
    na.rm = TRUE
  )
)


cat("\nTotal de defunciones 2012 mujeres:\n")

print(
  sum(
    prueba_2012_mujeres$defunciones,
    na.rm = TRUE
  )
)

# ============================================================
# 6.18. VALIDACIÓN DE INTEGRIDAD DE LAS BASES DE PRUEBA
# ============================================================

# ------------------------------------------------------------
# 6.18.1. Identificar combinaciones de años y provincias
# ------------------------------------------------------------

cat("\nCombinaciones provincia-declaración-defunción:\n")

combinaciones_prueba <- bind_rows(
  prueba_2022_hombres,
  prueba_2012_mujeres
) %>%
  distinct(
    universo,
    sexo,
    provincia_id,
    anio_declaracion,
    anio_defuncion
  )

print(
  combinaciones_prueba %>%
    count(universo, sexo)
)


# ------------------------------------------------------------
# 6.18.2. Comprobar duplicados en las claves de análisis
# ------------------------------------------------------------

cat("\nDuplicados 2022 hombres:\n")

duplicados_2022 <- prueba_2022_hombres %>%
  count(
    universo,
    sexo,
    provincia_id,
    anio_declaracion,
    anio_defuncion,
    edad_quinquenal
  ) %>%
  filter(n > 1)

print(duplicados_2022)


cat("\nDuplicados 2012 mujeres:\n")

duplicados_2012 <- prueba_2012_mujeres %>%
  count(
    universo,
    sexo,
    provincia_id,
    anio_declaracion,
    anio_defuncion,
    edad_quinquenal
  ) %>%
  filter(n > 1)

print(duplicados_2012)


# ------------------------------------------------------------
# 6.18.3. Comprobar 23 categorías por combinación
# ------------------------------------------------------------

cat("\nNúmero de categorías por combinación: 2022 hombres\n")

categorias_2022 <- prueba_2022_hombres %>%
  group_by(
    provincia_id,
    anio_declaracion,
    anio_defuncion
  ) %>%
  summarise(
    n_edades = n_distinct(edad_quinquenal),
    n_filas = n(),
    .groups = "drop"
  )

print(
  categorias_2022 %>%
    count(n_edades, n_filas)
)


cat("\nNúmero de categorías por combinación: 2012 mujeres\n")

categorias_2012 <- prueba_2012_mujeres %>%
  group_by(
    provincia_id,
    anio_declaracion,
    anio_defuncion
  ) %>%
  summarise(
    n_edades = n_distinct(edad_quinquenal),
    n_filas = n(),
    .groups = "drop"
  )

print(
  categorias_2012 %>%
    count(n_edades, n_filas)
)


# ------------------------------------------------------------
# 6.18.4. Comprobar correspondencia entre filas y combinaciones
# ------------------------------------------------------------

cat("\nNúmero esperado de filas 2022 hombres:\n")

print(
  nrow(categorias_2022) * length(edades_esperadas)
)

cat("\nNúmero observado de filas 2022 hombres:\n")

print(
  nrow(prueba_2022_hombres)
)


cat("\nNúmero esperado de filas 2012 mujeres:\n")

print(
  nrow(categorias_2012) * length(edades_esperadas)
)

cat("\nNúmero observado de filas 2012 mujeres:\n")

print(
  nrow(prueba_2012_mujeres)
)


# ------------------------------------------------------------
# 6.18.5.1 Validar disponibilidad de Edad ignorada
# ------------------------------------------------------------

cat(
  "\nDisponibilidad de Edad ignorada:\n"
)


disponibilidad_edad_ignorada <- bind_rows(
  prueba_2022_hombres,
  prueba_2012_mujeres
) %>%
  filter(
    edad_quinquenal == "Edad ignorada"
  ) %>%
  group_by(
    universo,
    sexo,
    categoria_disponible
  ) %>%
  summarise(
    filas = n(),
    valores_faltantes = sum(is.na(defunciones)),
    recuentos_observados = sum(!is.na(defunciones)),
    .groups = "drop"
  )


print(disponibilidad_edad_ignorada)


# Comprobar disponibilidad de la categoría.

stopifnot(
  
  all(
    prueba_2022_hombres$categoria_disponible
  ),
  
  all(
    !prueba_2012_mujeres$categoria_disponible[
      prueba_2012_mujeres$edad_quinquenal ==
        "Edad ignorada"
    ]
  ),
  
  all(
    is.na(
      prueba_2012_mujeres$defunciones[
        prueba_2012_mujeres$edad_quinquenal ==
          "Edad ignorada"
      ]
    )
  )
  
)


# ------------------------------------------------------------
# 6.18.5.2 Validar resultados
# ------------------------------------------------------------

stopifnot(
  nrow(duplicados_2022) == 0,
  nrow(duplicados_2012) == 0,
  all(categorias_2022$n_edades == 23),
  all(categorias_2022$n_filas == 23),
  all(categorias_2012$n_edades == 23),
  all(categorias_2012$n_filas == 23),
  nrow(prueba_2022_hombres) ==
    nrow(categorias_2022) * 23,
  nrow(prueba_2012_mujeres) ==
    nrow(categorias_2012) * 23
)

cat(
  "\nVALIDACIÓN DE INTEGRIDAD SUPERADA.\n"
)

# ------------------------------------------------------------
# 6.18.5.3. Validar disponibilidad de las demás categorías
# ------------------------------------------------------------

categorias_disponibles <- bind_rows(
  prueba_2022_hombres,
  prueba_2012_mujeres
) %>%
  filter(
    edad_quinquenal != "Edad ignorada"
  )

stopifnot(
  all(categorias_disponibles$categoria_disponible),
  !anyNA(categorias_disponibles$defunciones)
)


# ============================================================
# 6.19. VALIDACIÓN DE TOTALES ORIGINALES DE REDATAM
# ============================================================

# OBJETIVO:
#
# Comparar los totales originales de REDATAM con:
#
# 1. La suma de las categorías de edad en cada fila original.
# 2. La suma de las categorías de edad disponibles después
#    de ejecutar la función leer_redatam().
#
# Se utilizan los archivos de prueba:
# - 2022_hombres.xlsx
# - 2012_mujeres.xlsx
#
# Esta sección no modifica leer_redatam().


# ------------------------------------------------------------
# 6.19.1. FUNCIÓN AUXILIAR PARA CONVERTIR VALORES NUMÉRICOS
# ------------------------------------------------------------

convertir_valor_redatam <- function(x) {
  
  x <- str_trim(as.character(x))
  
  case_when(
    
    is.na(x) | x == "" ~ NA_real_,
    
    x == "-" ~ 0,
    
    TRUE ~ suppressWarnings(
      as.numeric(
        str_replace_all(
          x,
          ",",
          ""
        )
      )
    )
  )
}


# ------------------------------------------------------------
# 6.19.2. FUNCIÓN PARA RECUPERAR LOS TOTALES ORIGINALES
# ------------------------------------------------------------

recuperar_totales_redatam <- function(archivo) {
  
  # ----------------------------------------------------------
  # 1. Leer el archivo original de REDATAM
  # ----------------------------------------------------------
  
  datos <- read_excel(
    archivo,
    skip = 13,
    col_names = FALSE,
    col_types = "text",
    .name_repair = "minimal"
  )
  
  numero_columnas <- ncol(datos)
  
  nombre_archivo <- basename(archivo)
  
  names(datos) <- paste0(
    "v",
    seq_len(numero_columnas)
  )
  
  
  # ----------------------------------------------------------
  # 2. Identificar estrictamente la excepción de 2012 mujeres
  # ----------------------------------------------------------
  
  es_2012_mujeres <- grepl(
    "^2012[ _-]*mujeres?(\\s*\\(\\d+\\))?\\.xlsx$",
    nombre_archivo,
    ignore.case = TRUE
  )
  
  
  # ----------------------------------------------------------
  # 3. Validar el número de columnas esperado
  # ----------------------------------------------------------
  
  if (es_2012_mujeres && numero_columnas != 26L) {
    
    stop(
      paste(
        "El archivo 2012 mujeres debe tener 26 columnas:",
        nombre_archivo
      )
    )
    
  }
  
  if (!es_2012_mujeres && numero_columnas != 27L) {
    
    stop(
      paste(
        "El archivo debe tener 27 columnas:",
        nombre_archivo
      )
    )
    
  }
  
  
  # ----------------------------------------------------------
  # 4. Definir las columnas de edad y la columna del total
  # ----------------------------------------------------------
  
  if (es_2012_mujeres) {
    
    columnas_edad <- paste0("v", 4:25)
    columna_total <- "v26"
    
  } else {
    
    columnas_edad <- paste0("v", 4:26)
    columna_total <- "v27"
    
  }
  
  
  # ----------------------------------------------------------
  # 5. Excluir RESUMEN y todas las filas posteriores
  # ----------------------------------------------------------
  
  datos <- datos %>%
    mutate(
      es_resumen = coalesce(
        str_detect(
          v2,
          regex("^RESUMEN$", ignore_case = TRUE)
        ),
        FALSE
      ),
      
      excluir_resumen = cumsum(
        as.integer(es_resumen)
      ) > 0L
    ) %>%
    filter(!excluir_resumen)
  
  
  # ----------------------------------------------------------
  # 6. Identificar las provincias
  # ----------------------------------------------------------
  
  datos <- datos %>%
    mutate(
      provincia_id = if_else(
        coalesce(
          str_detect(v2, "^AREA\\s*#"),
          FALSE
        ),
        suppressWarnings(as.integer(v3)),
        NA_integer_
      )
    ) %>%
    fill(
      provincia_id,
      .direction = "down"
    )
  
  
  # Validar las siete provincias.
  
  provincias_en_archivo <- sort(
    unique(
      na.omit(datos$provincia_id)
    )
  )
  
  if (!identical(provincias_en_archivo, 1:7)) {
    
    stop(
      paste(
        "La estructura provincial no contiene las siete provincias:",
        nombre_archivo
      )
    )
    
  }
  
  
  # ----------------------------------------------------------
  # 7. Identificar encabezados y bloques acumulados
  # ----------------------------------------------------------
  
  datos <- datos %>%
    mutate(
      es_encabezado = coalesce(
        str_detect(
          v4,
          regex(
            "^Menores de 1 año$",
            ignore_case = TRUE
          )
        ),
        FALSE
      ),
      
      bloque_acumulado = es_encabezado &
        coalesce(
          str_detect(
            v2,
            regex("^Total$", ignore_case = TRUE)
          ),
          FALSE
        )
    )
  
  
  # ----------------------------------------------------------
  # 8. Excluir los bloques acumulados
  # ----------------------------------------------------------
  
  datos <- datos %>%
    group_by(provincia_id) %>%
    mutate(
      excluir_acumulado = cumsum(
        as.integer(bloque_acumulado)
      ) > 0L
    ) %>%
    ungroup() %>%
    filter(!excluir_acumulado)
  
  
  # ----------------------------------------------------------
  # 9. Identificar y propagar el año de declaración
  # ----------------------------------------------------------
  
  datos <- datos %>%
    mutate(
      anio_declaracion = if_else(
        es_encabezado &
          coalesce(
            str_detect(v2, "^\\d{4}$"),
            FALSE
          ),
        suppressWarnings(as.integer(v2)),
        NA_integer_
      )
    ) %>%
    group_by(provincia_id) %>%
    fill(
      anio_declaracion,
      .direction = "down"
    ) %>%
    ungroup()
  
  
  # ----------------------------------------------------------
  # 10. Identificar el año de defunción
  # ----------------------------------------------------------
  
  datos <- datos %>%
    mutate(
      anio_defuncion = if_else(
        coalesce(
          str_detect(v3, "^\\d{4}$"),
          FALSE
        ),
        suppressWarnings(as.integer(v3)),
        NA_integer_
      )
    ) %>%
    filter(!is.na(anio_defuncion))
  
  
  # ----------------------------------------------------------
  # 11. Convertir las categorías de edad y el total
  # ----------------------------------------------------------
  
  datos <- datos %>%
    mutate(
      across(
        all_of(columnas_edad),
        convertir_valor_redatam
      ),
      
      total_redatam = convertir_valor_redatam(
        .data[[columna_total]]
      )
    )
  
  
  # ----------------------------------------------------------
  # 12. Calcular la suma original de las categorías de edad
  # ----------------------------------------------------------
  
  datos <- datos %>%
    mutate(
      suma_edades_original = rowSums(
        pick(all_of(columnas_edad)),
        na.rm = FALSE
      )
    )
  
  
  # ----------------------------------------------------------
  # 13. Validar que las claves estén completas
  # ----------------------------------------------------------
  
  if (
    anyNA(datos$provincia_id) ||
    anyNA(datos$anio_declaracion) ||
    anyNA(datos$anio_defuncion)
  ) {
    
    stop(
      paste(
        "Se encontraron claves incompletas al recuperar los totales:",
        nombre_archivo
      )
    )
    
  }
  
  
  # ----------------------------------------------------------
  # 14. Conservar las variables necesarias
  # ----------------------------------------------------------
  
  datos %>%
    select(
      provincia_id,
      anio_declaracion,
      anio_defuncion,
      total_redatam,
      suma_edades_original
    )
  
}


# ------------------------------------------------------------
# 6.19.3. RECUPERAR LOS TOTALES ORIGINALES DE LOS DOS ARCHIVOS
# ------------------------------------------------------------

totales_originales_2022 <- recuperar_totales_redatam(
  archivo_2022_hombres
)

totales_originales_2012 <- recuperar_totales_redatam(
  archivo_2012_mujeres
)


# ------------------------------------------------------------
# 6.19.4. VALIDAR SUMA DE EDADES CONTRA TOTAL ORIGINAL
# ------------------------------------------------------------

comparacion_original_2022 <- totales_originales_2022 %>%
  mutate(
    diferencia_original =
      suma_edades_original - total_redatam
  )

comparacion_original_2012 <- totales_originales_2012 %>%
  mutate(
    diferencia_original =
      suma_edades_original - total_redatam
  )


# Resumen de 2022 hombres.

cat("\nResumen de comparación original: 2022 hombres\n")

print(
  comparacion_original_2022 %>%
    summarise(
      filas = n(),
      totales_faltantes = sum(is.na(total_redatam)),
      sumas_faltantes = sum(is.na(suma_edades_original)),
      coincidencias = sum(
        diferencia_original == 0,
        na.rm = TRUE
      ),
      diferencias = sum(
        diferencia_original != 0,
        na.rm = TRUE
      ),
      diferencias_no_evaluables = sum(
        is.na(diferencia_original)
      )
    )
)


# Resumen de 2012 mujeres.

cat("\nResumen de comparación original: 2012 mujeres\n")

print(
  comparacion_original_2012 %>%
    summarise(
      filas = n(),
      totales_faltantes = sum(is.na(total_redatam)),
      sumas_faltantes = sum(is.na(suma_edades_original)),
      coincidencias = sum(
        diferencia_original == 0,
        na.rm = TRUE
      ),
      diferencias = sum(
        diferencia_original != 0,
        na.rm = TRUE
      ),
      diferencias_no_evaluables = sum(
        is.na(diferencia_original)
      )
    )
)


# ------------------------------------------------------------
# 6.19.5. MOSTRAR DIFERENCIAS ORIGINALES, SI EXISTEN
# ------------------------------------------------------------

cat("\nDiferencias originales: 2022 hombres\n")

print(
  comparacion_original_2022 %>%
    filter(
      !is.na(diferencia_original),
      diferencia_original != 0
    )
)


cat("\nDiferencias originales: 2012 mujeres\n")

print(
  comparacion_original_2012 %>%
    filter(
      !is.na(diferencia_original),
      diferencia_original != 0
    )
)


# ------------------------------------------------------------
# 6.19.6. FUNCIÓN PARA COMPARAR TOTALES ORIGINALES Y PROCESADOS
# ------------------------------------------------------------

comparar_totales_procesados <- function(
    totales_originales,
    datos_procesados
) {
  
  # ----------------------------------------------------------
  # 1. Calcular totales de las bases procesadas
  # ----------------------------------------------------------
  
  totales_procesados <- datos_procesados %>%
    group_by(
      provincia_id,
      anio_declaracion,
      anio_defuncion
    ) %>%
    summarise(
      
      categorias_no_disponibles = sum(
        !categoria_disponible
      ),
      
      faltantes_disponibles = sum(
        categoria_disponible & is.na(defunciones)
      ),
      
      total_procesado = if (
        any(categoria_disponible & is.na(defunciones))
      ) {
        
        NA_real_
        
      } else {
        
        sum(
          defunciones[categoria_disponible]
        )
        
      },
      
      .groups = "drop"
    )
  
  
  # ----------------------------------------------------------
  # 2. Validar faltantes en categorías disponibles
  # ----------------------------------------------------------
  
  if (
    any(totales_procesados$faltantes_disponibles > 0)
  ) {
    
    stop(
      "Existen valores faltantes en categorías disponibles."
    )
    
  }
  
  
  # ----------------------------------------------------------
  # 3. Validar claves únicas en ambas bases
  # ----------------------------------------------------------
  
  claves <- c(
    "provincia_id",
    "anio_declaracion",
    "anio_defuncion"
  )
  
  if (
    anyDuplicated(
      totales_originales %>%
      select(all_of(claves))
    ) > 0
  ) {
    
    stop(
      "Existen claves duplicadas en los totales originales."
    )
    
  }
  
  if (
    anyDuplicated(
      totales_procesados %>%
      select(all_of(claves))
    ) > 0
  ) {
    
    stop(
      "Existen claves duplicadas en los totales procesados."
    )
    
  }
  
  
  # ----------------------------------------------------------
  # 4. Añadir indicadores de presencia
  # ----------------------------------------------------------
  
  originales_con_presencia <- totales_originales %>%
    mutate(
      presente_original = TRUE
    )
  
  procesados_con_presencia <- totales_procesados %>%
    mutate(
      presente_procesado = TRUE
    )
  
  
  # ----------------------------------------------------------
  # 5. Comparar ambas bases en las dos direcciones
  # ----------------------------------------------------------
  
  resultado <- full_join(
    originales_con_presencia,
    procesados_con_presencia,
    by = claves,
    suffix = c("_original", "_procesado")
  ) %>%
    mutate(
      falta_original = is.na(presente_original),
      
      falta_procesado = is.na(presente_procesado),
      
      diferencia_procesamiento =
        total_procesado - total_redatam
    )
  
  
  # ----------------------------------------------------------
  # 6. Devolver resultados
  # ----------------------------------------------------------
  
  resultado
  
}


# ------------------------------------------------------------
# 6.19.6.1. EJECUTAR COMPARACIÓN PARA LOS DOS ARCHIVOS
# ------------------------------------------------------------

comparacion_procesamiento_2022 <-
  comparar_totales_procesados(
    totales_originales_2022,
    prueba_2022_hombres
  )


comparacion_procesamiento_2012 <-
  comparar_totales_procesados(
    totales_originales_2012,
    prueba_2012_mujeres
  )


# ------------------------------------------------------------
# 6.19.6.2. VALIDAR CORRESPONDENCIA DE LAS COMBINACIONES
# ------------------------------------------------------------

if (
  any(comparacion_procesamiento_2022$falta_original) ||
  any(comparacion_procesamiento_2022$falta_procesado)
) {
  
  stop(
    paste(
      "Las combinaciones originales y procesadas no coinciden",
      "en 2022 hombres."
    )
  )
  
}


if (
  any(comparacion_procesamiento_2012$falta_original) ||
  any(comparacion_procesamiento_2012$falta_procesado)
) {
  
  stop(
    paste(
      "Las combinaciones originales y procesadas no coinciden",
      "en 2012 mujeres."
    )
  )
  
}


cat(
  "\nTodas las combinaciones originales y procesadas coinciden.\n"
)


# ------------------------------------------------------------
# 6.19.7. RESUMEN DE COMPARACIÓN DEL PROCESAMIENTO
# ------------------------------------------------------------

cat("\nComparación del procesamiento: 2022 hombres\n")

print(
  comparacion_procesamiento_2022 %>%
    summarise(
      filas = n(),
      totales_faltantes = sum(is.na(total_redatam)),
      procesados_faltantes = sum(is.na(total_procesado)),
      coincidencias = sum(
        diferencia_procesamiento == 0,
        na.rm = TRUE
      ),
      diferencias = sum(
        diferencia_procesamiento != 0,
        na.rm = TRUE
      ),
      diferencias_no_evaluables = sum(
        is.na(diferencia_procesamiento)
      ),
      combinaciones_sin_original = sum(falta_original),
      combinaciones_sin_procesado = sum(falta_procesado)
    )
)


cat("\nComparación del procesamiento: 2012 mujeres\n")

print(
  comparacion_procesamiento_2012 %>%
    summarise(
      filas = n(),
      totales_faltantes = sum(is.na(total_redatam)),
      procesados_faltantes = sum(is.na(total_procesado)),
      coincidencias = sum(
        diferencia_procesamiento == 0,
        na.rm = TRUE
      ),
      diferencias = sum(
        diferencia_procesamiento != 0,
        na.rm = TRUE
      ),
      diferencias_no_evaluables = sum(
        is.na(diferencia_procesamiento)
      ),
      combinaciones_sin_original = sum(falta_original),
      combinaciones_sin_procesado = sum(falta_procesado)
    )
)


# ------------------------------------------------------------
# 6.19.8. MOSTRAR DIFERENCIAS DEL PROCESAMIENTO, SI EXISTEN
# ------------------------------------------------------------

cat("\nDiferencias de procesamiento: 2022 hombres\n")

print(
  comparacion_procesamiento_2022 %>%
    filter(
      !is.na(diferencia_procesamiento),
      diferencia_procesamiento != 0
    )
)


cat("\nDiferencias de procesamiento: 2012 mujeres\n")

print(
  comparacion_procesamiento_2012 %>%
    filter(
      !is.na(diferencia_procesamiento),
      diferencia_procesamiento != 0
    )
)


# ------------------------------------------------------------
# 6.19.9. VALIDACIÓN FINAL DE LOS TOTALES PROCESADOS
# ------------------------------------------------------------

if (
  anyNA(comparacion_procesamiento_2022$diferencia_procesamiento) ||
  anyNA(comparacion_procesamiento_2012$diferencia_procesamiento)
) {
  
  stop(
    paste(
      "Existen comparaciones no evaluables entre los totales",
      "originales y procesados."
    )
  )
  
}


if (
  any(
    comparacion_procesamiento_2022$diferencia_procesamiento != 0
  ) ||
  any(
    comparacion_procesamiento_2012$diferencia_procesamiento != 0
  )
) {
  
  stop(
    paste(
      "Se detectaron diferencias entre los totales",
      "originales y procesados."
    )
  )
  
}


cat(
  "\n============================================================\n"
)

cat(
  "VALIDACIÓN FINAL DE TOTALES SUPERADA.\n"
)

cat(
  "============================================================\n"
)


# ============================================================
# 6.19.10. VERIFICACIÓN DE LA DEFINICIÓN ACTUAL DE leer_redatam()
# ============================================================

cat(
  "\n============================================================\n"
)

cat(
  "DEFINICIÓN ACTUAL DE leer_redatam()\n"
)

cat(
  "============================================================\n"
)


if (!exists("leer_redatam")) {
  
  stop(
    "La función leer_redatam() no existe en el entorno."
  )
  
}


cat("\nClase del objeto:\n")

print(class(leer_redatam))


cat("\nEntorno de la función:\n")

print(environment(leer_redatam))


cat("\nCódigo completo de la función:\n")

print(leer_redatam)


cat("\nCódigo de la función como texto:\n")

cat(
  paste(
    deparse(leer_redatam),
    collapse = "\n"
  )
)



# ============================================================
# 7. INTEGRACIÓN DE LOS 52 ARCHIVOS REDATAM
# ============================================================

# OBJETIVO:
# Procesar los 52 archivos mediante leer_redatam()
# e integrar los resultados en una única base:
# datos_completos.
#
# Se conserva la estructura:
## universo, sexo, provincia_id, provincia,
# anio_declaracion, anio_defuncion,
# edad_quinquenal, categoria_disponible,
# defunciones.
#
# IMPORTANTE:
# Los universos representan archivos de distintos años.
# Por tanto, los totales integrados no deben interpretarse
# como defunciones únicas de todo el período.
#
# EXCEPCIÓN ESTRUCTURAL:
# El archivo 2012_mujeres.xlsx contiene 22 categorías
# de edad observadas. No dispone de la categoría
# "Edad ignorada".
#
# Para conservar la estructura homogénea de 23 categorías:
# - categoria_disponible = FALSE para Edad ignorada.
# - defunciones = NA para esa categoría.
#
# En los otros 51 archivos las 23 categorías están disponibles.


# ------------------------------------------------------------
# 7.1. VERIFICAR METADATOS ANTES DE INTEGRAR
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("7.1. VALIDACIÓN PREVIA DE LOS METADATOS\n")
cat("============================================================\n")


# Número de archivos.

cat("\nNúmero de archivos identificados:\n")

print(nrow(meta_archivos))


# Años de universo.

cat("\nAños de universo:\n")

print(
  sort(unique(meta_archivos$universo))
)


# Distribución por sexo.

cat("\nDistribución por sexo:\n")

print(
  meta_archivos %>%
    count(sexo)
)


# Combinaciones universo-sexo.

cat("\nCombinaciones universo-sexo:\n")

print(
  meta_archivos %>%
    count(universo, sexo) %>%
    count(n)
)


# Identificar posibles duplicados de metadatos.

duplicados_archivos <- meta_archivos %>%
  count(universo, sexo) %>%
  filter(n != 1)


cat("\nCombinaciones universo-sexo duplicadas o faltantes:\n")

print(duplicados_archivos)


# Validaciones previas.

stopifnot(
  nrow(meta_archivos) == 52,
  n_distinct(meta_archivos$universo) == 26,
  setequal(
    sort(unique(meta_archivos$universo)),
    2000:2025
  ),
  setequal(
    unique(meta_archivos$sexo),
    c("Hombre", "Mujer")
  ),
  nrow(duplicados_archivos) == 0
)


cat("\nVALIDACIÓN PREVIA SUPERADA.\n")


# ============================================================
# 7.2. PROCESAMIENTO Y AUDITORÍA DE LOS 52 ARCHIVOS
# ============================================================

# OBJETIVO:
#
# 1. Ejecutar leer_redatam() sobre los 52 archivos.
# 2. Auditar individualmente los resultados.
# 3. Comprobar la estructura de las 23 categorías de edad.
# 4. Identificar valores faltantes legítimos y no legítimos.
# 5. Verificar duplicados, provincias y correspondencia
#    entre universo y sexo.
#
# IMPORTANTE:
#
# La categoría Edad ignorada no está disponible en el archivo
# de mujeres de 2012. Sus valores NA son estructurales y
# no deben considerarse errores de procesamiento.
#
# Este bloque todavía no integra las bases.


# ------------------------------------------------------------
# 7.2.1. Procesar individualmente los 52 archivos
# ------------------------------------------------------------

bases_archivos <- purrr::pmap(
  meta_archivos %>%
    select(
      archivo,
      universo,
      sexo
    ),
  
  function(archivo, universo, sexo) {
    
    leer_redatam(
      archivo = archivo,
      universo = universo,
      sexo = sexo
    )
    
  }
)


# Comprobar el número de archivos procesados.

cat("\nNúmero de bases procesadas:\n")

print(length(bases_archivos))

stopifnot(
  length(bases_archivos) == nrow(meta_archivos),
  length(bases_archivos) == 52
)


# ------------------------------------------------------------
# 7.2.2. Construir auditoría individual
# ------------------------------------------------------------

auditoria_archivos <- purrr::map2_dfr(
  seq_len(nrow(meta_archivos)),
  bases_archivos,
  
  function(i, base) {
    
    # Metadatos esperados del archivo.
    
    universo_esperado <- meta_archivos$universo[i]
    
    sexo_esperado <- meta_archivos$sexo[i]
    
    excepcion_2012_mujeres <- (
      universo_esperado == 2012L &&
        sexo_esperado == "Mujer"
    )
    
    
    # Comprobar integridad de las combinaciones.
    
    integridad_combinaciones <- base %>%
      group_by(
        provincia_id,
        anio_declaracion,
        anio_defuncion
      ) %>%
      summarise(
        n_filas = n(),
        n_edades = n_distinct(edad_quinquenal),
        orden_edades_correcto = identical(
          as.character(edad_quinquenal),
          edades_esperadas
        ),
        .groups = "drop"
      )
    
    
    # Identificar duplicados en la clave de análisis.
    
    duplicados <- base %>%
      count(
        universo,
        sexo,
        provincia_id,
        anio_declaracion,
        anio_defuncion,
        edad_quinquenal
      ) %>%
      filter(n > 1)
    
    
    # Comprobar la disponibilidad de las categorías.
    
    disponibilidad_correcta <- all(
      base$categoria_disponible ==
        !(
          excepcion_2012_mujeres &
            base$edad_quinquenal == "Edad ignorada"
        )
    )
    
    
    # Separar valores faltantes estructurales
    # de valores faltantes inesperados.
    
    faltantes_no_disponibles <- sum(
      is.na(base$defunciones) &
        !base$categoria_disponible
    )
    
    faltantes_disponibles <- sum(
      is.na(base$defunciones) &
        base$categoria_disponible
    )
    
    faltantes_estructurales <- sum(
      is.na(
        base %>%
          select(
            -defunciones,
            -valor_original
          )
      )
    )
    
    # Verificar que las categorías no disponibles
    # tengan NA y las disponibles tengan recuentos.
    
    valores_disponibilidad_correctos <- (
      all(
        is.na(
          base$defunciones[
            !base$categoria_disponible
          ]
        )
      ) &&
        all(
          !is.na(
            base$defunciones[
              base$categoria_disponible
            ]
          )
        )
    )
    
    
    # Construir registro de auditoría.
    
    tibble(
      
      archivo = basename(
        meta_archivos$archivo[i]
      ),
      
      universo_esperado = universo_esperado,
      
      sexo_esperado = sexo_esperado,
      
      filas = nrow(base),
      
      universo_correcto = all(
        base$universo == universo_esperado
      ),
      
      sexo_correcto = all(
        base$sexo == sexo_esperado
      ),
      
      provincias = n_distinct(
        base$provincia_id
      ),
      
      categorias = n_distinct(
        base$edad_quinquenal
      ),
      
      categorias_correctas = setequal(
        unique(base$edad_quinquenal),
        edades_esperadas
      ),
      
      combinaciones = nrow(
        integridad_combinaciones
      ),
      
      combinaciones_23_edades = sum(
        integridad_combinaciones$n_filas == 23 &
          integridad_combinaciones$n_edades == 23
      ),
      
      orden_edades_correcto = all(
        integridad_combinaciones$orden_edades_correcto
      ),
      
      duplicados = nrow(
        duplicados
      ),
      
      disponibilidad_correcta =
        disponibilidad_correcta,
      
      valores_disponibles_faltantes =
        faltantes_disponibles,
      
      valores_no_disponibles_faltantes =
        faltantes_no_disponibles,
      
      faltantes_estructurales =
        faltantes_estructurales,
      
      valores_disponibilidad_correctos =
        valores_disponibilidad_correctos,
      
      defunciones_negativas = sum(
        base$defunciones < 0,
        na.rm = TRUE
      )
      
    )
    
  }
)


# ------------------------------------------------------------
# 7.2.3. Examinar resultados de la auditoría
# ------------------------------------------------------------

cat("\nDimensiones de la auditoría:\n")

print(dim(auditoria_archivos))


cat("\nAuditoría individual de los archivos:\n")

print(auditoria_archivos)


# ------------------------------------------------------------
# 7.2.4. Identificar archivos con problemas
# ------------------------------------------------------------

archivos_con_problemas <- auditoria_archivos %>%
  filter(
    
    !universo_correcto |
      
      !sexo_correcto |
      
      provincias != 7 |
      
      categorias != 23 |
      
      !categorias_correctas |
      
      combinaciones != combinaciones_23_edades |
      
      !orden_edades_correcto |
      
      duplicados > 0 |
      
      !disponibilidad_correcta |
      
      valores_disponibles_faltantes > 0 |
      
      !valores_disponibilidad_correctos |
      
      faltantes_estructurales > 0 |
      
      defunciones_negativas > 0
    
  )


cat("\nArchivos con problemas de procesamiento:\n")

print(archivos_con_problemas)


# ------------------------------------------------------------
# 7.2.5. Validación global de los 52 archivos
# ------------------------------------------------------------

stopifnot(
  
  nrow(auditoria_archivos) == 52,
  
  all(auditoria_archivos$universo_correcto),
  
  all(auditoria_archivos$sexo_correcto),
  
  all(auditoria_archivos$provincias == 7),
  
  all(auditoria_archivos$categorias == 23),
  
  all(auditoria_archivos$categorias_correctas),
  
  all(
    auditoria_archivos$combinaciones ==
      auditoria_archivos$combinaciones_23_edades
  ),
  
  all(auditoria_archivos$orden_edades_correcto),
  
  all(auditoria_archivos$duplicados == 0),
  
  all(auditoria_archivos$disponibilidad_correcta),
  
  all(
    auditoria_archivos$valores_disponibles_faltantes == 0
  ),
  
  all(
    auditoria_archivos$valores_disponibilidad_correctos
  ),
  
  all(
    auditoria_archivos$faltantes_estructurales == 0
  ),
  
  all(
    auditoria_archivos$defunciones_negativas == 0
  ),
  
  nrow(archivos_con_problemas) == 0
  
)

cat(
  "\nVALIDACIÓN DE LOS 52 ARCHIVOS SUPERADA.\n"
)


# ============================================================
# 7.3. INTEGRACIÓN DE LAS 52 BASES PROCESADAS
# ============================================================

# OBJETIVO:
#
# Integrar las 52 bases individuales, previamente procesadas
# y auditadas, en una única base de datos.
#
# La integración se realiza únicamente después de comprobar
# que los 52 archivos superaron la auditoría individual.
#
# IMPORTANTE:
#
# La variable categoria_disponible identifica las categorías
# de edad que efectivamente existen en cada archivo.
#
# En mujeres de 2012, Edad ignorada no está disponible y sus
# valores deben conservarse como NA.


# ------------------------------------------------------------
# 7.3.1. Integrar las 52 bases individuales
# ------------------------------------------------------------

datos_completos <- bind_rows(
  bases_archivos
)


# ------------------------------------------------------------
# 7.3.2. Examinar las dimensiones y estructura
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("7.3. INTEGRACIÓN DE LOS ARCHIVOS\n")
cat("============================================================\n")


cat("\nDimensiones de datos_completos:\n")

print(
  dim(datos_completos)
)


cat("\nEstructura de datos_completos:\n")

glimpse(datos_completos)


# ============================================================
# 7.4. VALIDAR DIMENSIONES Y VARIABLES
# ============================================================

# OBJETIVO:
#
# Comprobar que la integración conserva:
# 1. Las once variables esperadas.
# 2. El orden de las variables.
# 3. Todas las filas de las 52 bases individuales.


# ------------------------------------------------------------
# 7.4.1. DEFINIR LAS VARIABLES ESPERADAS
# ------------------------------------------------------------

variables_esperadas <- c(
  "universo",
  "sexo",
  "provincia_id",
  "provincia",
  "anio_declaracion",
  "anio_defuncion",
  "edad_quinquenal",
  "categoria_disponible",
  "valor_original",
  "estado_registro",
  "defunciones"
)


# ------------------------------------------------------------
# 7.4.2. EXAMINAR LAS DIMENSIONES Y NOMBRES
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("7.4. VALIDACIÓN DE LA ESTRUCTURA INTEGRADA\n")
cat("============================================================\n")


cat("\nNombres de las variables:\n")

print(
  names(datos_completos)
)


cat("\nNúmero de filas:\n")

print(
  nrow(datos_completos)
)


cat("\nNúmero de columnas:\n")

print(
  ncol(datos_completos)
)


# ------------------------------------------------------------
# 7.4.3. VALIDAR LA ESTRUCTURA INTEGRADA
# ------------------------------------------------------------

stopifnot(
  
  identical(
    names(datos_completos),
    variables_esperadas
  ),
  
  ncol(datos_completos) == 11,
  
  nrow(datos_completos) ==
    sum(auditoria_archivos$filas)
  
)


cat(
  "\nVALIDACIÓN DE LA ESTRUCTURA INTEGRADA SUPERADA.\n"
)


# ============================================================
# 7.5. VALIDAR COBERTURA DE UNIVERSOS, SEXOS Y PROVINCIAS
# ============================================================

# OBJETIVO:
#
# Comprobar que la integración conserva:
# 1. Los 26 universos.
# 2. Los dos sexos.
# 3. Las 52 combinaciones universo-sexo.
# 4. Las siete provincias.
# 5. Las 364 combinaciones universo-sexo-provincia.
#
# Estas comprobaciones se realizan sobre la base integrada.
# No sustituyen ni repiten la auditoría individual de 7.2.


# ------------------------------------------------------------
# 7.5.1. Comprobar universos, sexos y combinaciones
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("7.5. COBERTURA DE UNIVERSOS, SEXOS Y PROVINCIAS\n")
cat("============================================================\n")


universos_observados <- sort(
  unique(datos_completos$universo)
)


sexos_observados <- sort(
  unique(datos_completos$sexo)
)


combinaciones_universo_sexo <- datos_completos %>%
  distinct(
    universo,
    sexo
  )


cat("\nUniversos identificados:\n")

print(universos_observados)


cat("\nSexos identificados:\n")

print(sexos_observados)


cat("\nNúmero de combinaciones universo-sexo:\n")

print(
  nrow(combinaciones_universo_sexo)
)


# ------------------------------------------------------------
# 7.5.2. Comprobar cobertura provincial
# ------------------------------------------------------------

provincias_observadas <- datos_completos %>%
  distinct(
    provincia_id,
    provincia
  ) %>%
  arrange(provincia_id)


cat("\nProvincias identificadas:\n")

print(provincias_observadas)


cobertura_provincias <- datos_completos %>%
  distinct(
    universo,
    sexo,
    provincia_id
  )


cat("\nNúmero de combinaciones universo-sexo-provincia:\n")

print(
  nrow(cobertura_provincias)
)


# ------------------------------------------------------------
# 7.5.3. Validación de la cobertura
# ------------------------------------------------------------

stopifnot(
  
  length(universos_observados) == 26,
  
  setequal(
    universos_observados,
    2000:2025
  ),
  
  length(sexos_observados) == 2,
  
  setequal(
    sexos_observados,
    c("Hombre", "Mujer")
  ),
  
  nrow(combinaciones_universo_sexo) == 52,
  
  n_distinct(
    combinaciones_universo_sexo$universo
  ) == 26,
  
  n_distinct(
    combinaciones_universo_sexo$sexo
  ) == 2,
  
  setequal(
    sort(unique(datos_completos$provincia_id)),
    1:7
  ),
  
  nrow(provincias_observadas) == 7,
  
  nrow(cobertura_provincias) == 364
  
)


cat(
  "\nVALIDACIÓN DE COBERTURA SUPERADA.\n"
)


# ============================================================
# 7.6. VALIDAR DISPONIBILIDAD, FALTANTES Y VALORES NEGATIVOS
# ============================================================

# OBJETIVO:
#
# Comprobar que:
# 1. categoria_disponible no contiene valores faltantes.
# 2. Las categorías disponibles tienen recuentos observados.
# 3. Las categorías no disponibles conservan valores NA.
# 4. La única categoría no disponible es Edad ignorada
#    en el archivo de mujeres de 2012.
# 5. No existen recuentos negativos.
#
# IMPORTANTE:
#
# Los NA de Edad ignorada en mujeres de 2012 son faltantes
# estructurales. No representan recuentos iguales a cero.


# ------------------------------------------------------------
# 7.6.1. Identificar faltantes en categorías disponibles
# ------------------------------------------------------------

faltantes_disponibles <- datos_completos %>%
  filter(
    categoria_disponible,
    is.na(defunciones)
  )


cat("\n============================================================\n")
cat("7.6. DISPONIBILIDAD, FALTANTES Y VALORES NEGATIVOS\n")
cat("============================================================\n")


cat("\nCategorías disponibles con recuentos faltantes:\n")

print(
  nrow(faltantes_disponibles)
)


# ------------------------------------------------------------
# 7.6.2. Identificar recuentos asignados a categorías no disponibles
# ------------------------------------------------------------

recuentos_no_disponibles <- datos_completos %>%
  filter(
    !categoria_disponible,
    !is.na(defunciones)
  )


cat("\nCategorías no disponibles con recuentos asignados:\n")

print(
  nrow(recuentos_no_disponibles)
)


# ------------------------------------------------------------
# 7.6.3. Identificar las categorías no disponibles
# ------------------------------------------------------------

categorias_no_disponibles <- datos_completos %>%
  filter(
    !categoria_disponible
  ) %>%
  distinct(
    universo,
    sexo,
    edad_quinquenal
  )


cat("\nCategorías estructuralmente no disponibles:\n")

print(
  categorias_no_disponibles
)


# ------------------------------------------------------------
# 7.6.4. Comprobar valores negativos
# ------------------------------------------------------------

defunciones_negativas <- sum(
  datos_completos$defunciones < 0,
  na.rm = TRUE
)


cat("\nNúmero de defunciones negativas:\n")

print(
  defunciones_negativas
)



# ------------------------------------------------------------
# 7.6.5. Validar disponibilidad y valores
# ------------------------------------------------------------

stopifnot(
  
  # categoria_disponible no debe contener valores faltantes.
  
  !anyNA(
    datos_completos$categoria_disponible
  ),
  
  
  # Todas las categorías distintas de Edad ignorada
  # deben estar disponibles.
  
  all(
    datos_completos$categoria_disponible[
      datos_completos$edad_quinquenal != "Edad ignorada"
    ]
  ),
  
  
  # No debe haber faltantes en categorías disponibles.
  
  nrow(faltantes_disponibles) == 0,
  
  
  # Las categorías no disponibles no deben tener
  # recuentos numéricos asignados.
  
  nrow(recuentos_no_disponibles) == 0,
  
  
  # Debe existir una sola combinación de universo,
  # sexo y categoría estructuralmente no disponible.
  
  nrow(categorias_no_disponibles) == 1,
  
  
  # La categoría no disponible debe corresponder
  # al universo 2012.
  
  all(
    categorias_no_disponibles$universo == 2012L
  ),
  
  
  # La categoría no disponible debe corresponder
  # al sexo Mujer.
  
  all(
    categorias_no_disponibles$sexo == "Mujer"
  ),
  
  
  # La única categoría no disponible debe ser
  # Edad ignorada.
  
  all(
    categorias_no_disponibles$edad_quinquenal ==
      "Edad ignorada"
  ),
  
  
  # Deben existir exactamente 42 registros
  # correspondientes a categorías no disponibles.
  
  sum(
    !datos_completos$categoria_disponible
  ) == 42,
  
  
  # No deben existir recuentos negativos.
  
  defunciones_negativas == 0
  
)


cat(
  "\nVALIDACIÓN DE DISPONIBILIDAD Y VALORES SUPERADA.\n"
)


# ============================================================
# 7.7. VALIDAR DUPLICADOS EN LA CLAVE DE ANÁLISIS
# ============================================================

# OBJETIVO:
#
# Comprobar que no existen registros duplicados en la base
# integrada para la siguiente clave:
#
# universo
# sexo
# provincia_id
# anio_declaracion
# anio_defuncion
# edad_quinquenal
#
# Esta comprobación permite verificar que la integración
# no haya generado superposiciones entre las bases individuales.


# ------------------------------------------------------------
# 7.7.1. Identificar duplicados
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("7.7. VALIDACIÓN DE DUPLICADOS\n")
cat("============================================================\n")


duplicados_integrados <- datos_completos %>%
  count(
    universo,
    sexo,
    provincia_id,
    anio_declaracion,
    anio_defuncion,
    edad_quinquenal
  ) %>%
  filter(
    n > 1
  )


cat("\nNúmero de claves duplicadas:\n")

print(
  nrow(duplicados_integrados)
)


cat("\nDetalle de duplicados:\n")

print(
  duplicados_integrados
)


# ------------------------------------------------------------
# 7.7.2. Validar ausencia de duplicados
# ------------------------------------------------------------

stopifnot(
  nrow(duplicados_integrados) == 0
)


cat(
  "\nVALIDACIÓN DE DUPLICADOS SUPERADA.\n"
)


# ============================================================
# 7.8. RESUMEN GENERAL DE LA BASE INTEGRADA
# ============================================================

# OBJETIVO:
#
# Describir las dimensiones y características principales
# de la base integrada.
#
# IMPORTANTE:
#
# La suma de defunciones representa la suma de los recuentos
# contenidos en los distintos universos REDATAM.
#
# No debe interpretarse como un número de personas fallecidas
# únicas ni como un total nacional de defunciones del período.


# ------------------------------------------------------------
# 7.8.1. Construir resumen de integración
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("7.8. RESUMEN GENERAL DE DATOS_COMPLETOS\n")
cat("============================================================\n")


resumen_integracion <- tibble(
  
  indicador = c(
    "Archivos procesados",
    "Universos",
    "Sexos",
    "Provincias",
    "Categorías de edad",
    "Filas integradas",
    "Columnas",
    "Filas correspondientes a categorías no disponibles",
    "Faltantes en categorías disponibles",
    "Suma de recuentos en categorías disponibles",
    "Defunciones negativas",
    "Claves duplicadas"
  ),
  
  valor = c(
    
    length(bases_archivos),
    
    n_distinct(
      datos_completos$universo
    ),
    
    n_distinct(
      datos_completos$sexo
    ),
    
    n_distinct(
      datos_completos$provincia_id
    ),
    
    n_distinct(
      datos_completos$edad_quinquenal
    ),
    
    nrow(datos_completos),
    
    ncol(datos_completos),
    
    sum(
      !datos_completos$categoria_disponible
    ),
    
    nrow(faltantes_disponibles),
    
    sum(
      datos_completos$defunciones[
        datos_completos$categoria_disponible
      ]
    ),
    
    defunciones_negativas,
    
    nrow(duplicados_integrados)
    
  )
  
)


cat("\nResumen de la integración:\n")

print(
  resumen_integracion
)


# ============================================================
# 7.9. VALIDACIÓN DE LOS TOTALES ORIGINALES DE LOS 52 ARCHIVOS
# ============================================================

# OBJETIVO:
#
# Comparar los totales recuperados directamente de los
# archivos REDATAM originales con los valores procesados
# e integrados en datos_completos.
#
# La comparación se realiza por:
#
# universo
# sexo
# provincia_id
# anio_declaracion
# anio_defuncion
#
# Se comprobarán:
#
# 1. La suma de las categorías de edad originales frente
#    al total informado por REDATAM.
#
# 2. La suma de las categorías disponibles procesadas
#    frente al total original de REDATAM.
#
# 3. La correspondencia de todas las combinaciones
#    entre los archivos originales y los procesados.
#
# IMPORTANTE:
#
# recuperar_totales_redatam() vuelve a leer el archivo
# original y aplica sus criterios de selección.
#
# Esta comparación verifica la conciliación aritmética
# entre las filas seleccionadas y la base procesada.
# Por sí sola no constituye una prueba independiente de
# que la selección de filas sea correcta.
#
# En mujeres de 2012 se consideran las 22 categorías
# disponibles. Edad ignorada no se interpreta como cero.


# ------------------------------------------------------------
# 7.9.1. Recuperar los totales originales de los 52 archivos
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("7.9.1. RECUPERACIÓN DE TOTALES ORIGINALES\n")
cat("============================================================\n")


totales_originales_52 <- purrr::pmap_dfr(
  
  meta_archivos %>%
    select(
      archivo,
      universo,
      sexo
    ),
  
  function(archivo, universo, sexo) {
    
    recuperar_totales_redatam(
      archivo = archivo
    ) %>%
      mutate(
        universo = universo,
        sexo = sexo
      )
    
  }
  
)


cat("\nDimensiones de los totales originales:\n")

print(
  dim(totales_originales_52)
)


cat("\nPrimeros registros:\n")

print(
  totales_originales_52 %>%
    slice_head(n = 10)
)


# ------------------------------------------------------------
# 7.9.2. Comprobar estructura y valores faltantes originales
# ------------------------------------------------------------

# Las claves y los totales recuperados deben estar completos.
# No se deben comparar combinaciones con claves o totales NA.


claves_comparacion <- c(
  "universo",
  "sexo",
  "provincia_id",
  "anio_declaracion",
  "anio_defuncion"
)


variables_originales_esperadas <- c(
  "provincia_id",
  "anio_declaracion",
  "anio_defuncion",
  "suma_edades_original",
  "total_redatam",
  "universo",
  "sexo"
)


stopifnot(
  
  all(
    variables_originales_esperadas %in%
      names(totales_originales_52)
  ),
  
  all(
    colSums(
      is.na(
        totales_originales_52 %>%
          select(
            all_of(
              c(
                claves_comparacion,
                "suma_edades_original",
                "total_redatam"
              )
            )
          )
      )
    ) == 0
  )
  
)


cat("\nValores faltantes en claves y totales originales:\n")

print(
  colSums(
    is.na(
      totales_originales_52 %>%
        select(
          all_of(
            c(
              claves_comparacion,
              "suma_edades_original",
              "total_redatam"
            )
          )
        )
    )
  )
)


# ------------------------------------------------------------
# 7.9.3. Comprobar duplicados en los totales originales
# ------------------------------------------------------------

duplicados_totales_originales <- totales_originales_52 %>%
  count(
    across(
      all_of(claves_comparacion)
    )
  ) %>%
  filter(
    n > 1
  )


cat("\nDuplicados en los totales originales:\n")

print(
  nrow(duplicados_totales_originales)
)


stopifnot(
  nrow(duplicados_totales_originales) == 0
)


# ------------------------------------------------------------
# 7.9.4. Comprobar que las edades suman el total REDATAM
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("7.9.4. SUMA DE EDADES FRENTE AL TOTAL REDATAM\n")
cat("============================================================\n")


comparacion_edades_originales <- totales_originales_52 %>%
  mutate(
    diferencia_edades_originales =
      suma_edades_original - total_redatam
  )


diferencias_edades_originales <- comparacion_edades_originales %>%
  filter(
    diferencia_edades_originales != 0
  )


cat("\nNúmero de combinaciones originales:\n")

print(
  nrow(comparacion_edades_originales)
)


cat("\nCombinaciones con diferencias entre edades y total:\n")

print(
  diferencias_edades_originales
)


# ------------------------------------------------------------
# 7.9.5. Sumar las categorías disponibles en datos_completos
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("7.9.5. SUMA DE EDADES EN DATOS_COMPLETOS\n")
cat("============================================================\n")


totales_procesados_52 <- datos_completos %>%
  
  group_by(
    across(
      all_of(claves_comparacion)
    )
  ) %>%
  
  summarise(
    
    categorias_no_disponibles =
      sum(!categoria_disponible),
    
    faltantes_disponibles =
      sum(
        categoria_disponible &
          is.na(defunciones)
      ),
    
    suma_edades_procesadas = if (
      any(
        categoria_disponible &
        is.na(defunciones)
      )
    ) {
      
      NA_real_
      
    } else {
      
      sum(
        defunciones[
          categoria_disponible
        ]
      )
      
    },
    
    .groups = "drop"
    
  )


cat("\nDimensiones de los totales procesados:\n")

print(
  dim(totales_procesados_52)
)


cat("\nCombinaciones con faltantes en categorías disponibles:\n")

print(
  totales_procesados_52 %>%
    filter(
      faltantes_disponibles > 0
    )
)


# ------------------------------------------------------------
# 7.9.6. Comprobar duplicados en los totales procesados
# ------------------------------------------------------------

duplicados_totales_procesados <- totales_procesados_52 %>%
  count(
    across(
      all_of(claves_comparacion)
    )
  ) %>%
  filter(
    n > 1
  )


cat("\nDuplicados en los totales procesados:\n")

print(
  nrow(duplicados_totales_procesados)
)


stopifnot(
  nrow(duplicados_totales_procesados) == 0
)


# ------------------------------------------------------------
# 7.9.7. Comparar totales originales y procesados
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("7.9.7. COMPARACIÓN DE TOTALES ORIGINALES Y PROCESADOS\n")
cat("============================================================\n")


comparacion_totales_52 <- full_join(
  
  comparacion_edades_originales,
  
  totales_procesados_52,
  
  by = claves_comparacion
  
) %>%
  
  mutate(
    
    falta_original =
      is.na(total_redatam),
    
    falta_procesado =
      is.na(suma_edades_procesadas),
    
    diferencia_procesada =
      suma_edades_procesadas - total_redatam
    
  )


cat("\nDimensiones de la comparación:\n")

print(
  dim(comparacion_totales_52)
)


# ------------------------------------------------------------
# 7.9.8. Identificar diferencias
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("7.9.8. IDENTIFICACIÓN DE DIFERENCIAS\n")
cat("============================================================\n")


diferencias_totales_52 <- comparacion_totales_52 %>%
  filter(
    
    falta_original |
      
      falta_procesado |
      
      faltantes_disponibles > 0 |
      
      is.na(diferencia_procesada) |
      
      diferencia_procesada != 0
    
  )


cat("\nNúmero de combinaciones comparadas:\n")

print(
  nrow(comparacion_totales_52)
)


cat("\nNúmero de combinaciones con diferencias:\n")

print(
  nrow(diferencias_totales_52)
)


cat("\nDetalle de las diferencias:\n")

print(
  diferencias_totales_52
)


# ------------------------------------------------------------
# 7.9.9. Resumen de la validación de totales
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("7.9.9. RESUMEN DE LA VALIDACIÓN DE TOTALES\n")
cat("============================================================\n")


resumen_totales_52 <- tibble(
  
  indicador = c(
    "Combinaciones recuperadas de REDATAM",
    "Combinaciones procesadas",
    "Combinaciones comparadas",
    "Diferencias entre edades originales y total REDATAM",
    "Combinaciones con diferencias o problemas de correspondencia",
    "Combinaciones sin registro original",
    "Combinaciones sin registro procesado"
  ),
  
  valor = c(
    
    nrow(totales_originales_52),
    
    nrow(totales_procesados_52),
    
    nrow(comparacion_totales_52),
    
    nrow(diferencias_edades_originales),
    
    nrow(diferencias_totales_52),
    
    sum(comparacion_totales_52$falta_original),
    
    sum(comparacion_totales_52$falta_procesado)
    
  )
  
)


print(
  resumen_totales_52
)


# ------------------------------------------------------------
# 7.9.10. Validación final de los totales originales
# ------------------------------------------------------------

stopifnot(
  
  nrow(diferencias_edades_originales) == 0,
  
  nrow(diferencias_totales_52) == 0,
  
  sum(
    comparacion_totales_52$falta_original
  ) == 0,
  
  sum(
    comparacion_totales_52$falta_procesado
  ) == 0,
  
  all(
    comparacion_totales_52$faltantes_disponibles == 0
  ),
  
  all(
    comparacion_totales_52$diferencia_edades_originales == 0
  ),
  
  all(
    comparacion_totales_52$diferencia_procesada == 0
  )
  
)


cat("\n============================================================\n")
cat("VALIDACIÓN DE LOS TOTALES ORIGINALES SUPERADA.\n")
cat("LOS 52 ARCHIVOS COINCIDEN CON LOS DATOS PROCESADOS.\n")
cat("============================================================\n")


# ============================================================
# 8. RESUMEN FINAL DE VALIDACIÓN POR ARCHIVO
# ============================================================

# OBJETIVO:
#
# Consolidar los resultados de las validaciones de la sección 7
# para cada uno de los 52 archivos REDATAM.
#
# Se comparan tres magnitudes:
#
# 1. Total procesado individualmente en cada archivo.
# 2. Total recuperado directamente del REDATAM original.
# 3. Total correspondiente en datos_completos.
#
# La comparación utiliza los resultados ya obtenidos en:
#
# - auditoria_archivos
# - comparacion_totales_52
#
# Esta sección no vuelve a leer los archivos originales.
#
# IMPORTANTE:
#
# Los totales representan recuentos acumulados en los archivos
# correspondientes a cada universo y sexo. No deben interpretarse
# como defunciones únicas al sumar universos diferentes.



# ------------------------------------------------------------
# 8.1. VERIFICAR LA IDENTIFICACIÓN DE LOS 52 ARCHIVOS
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("8.1. VERIFICACIÓN DE LOS ARCHIVOS Y SUS RESULTADOS\n")
cat("============================================================\n")


# Recuperar las claves de los archivos auditados.

claves_auditoria <- auditoria_archivos %>%
  transmute(
    universo = universo_esperado,
    sexo = sexo_esperado
  )


# Recuperar las claves de los archivos presentes
# en la comparación de totales.
#
# Cada universo y sexo puede aparecer en múltiples filas
# porque existen varias combinaciones provinciales y temporales.
# Por eso se utiliza distinct() antes de comprobar la cobertura.

claves_comparacion <- comparacion_totales_52 %>%
  distinct(
    universo,
    sexo
  )


# Comprobar que no existan archivos repetidos
# en la auditoría individual.

duplicados_auditoria <- claves_auditoria %>%
  count(
    universo,
    sexo
  ) %>%
  filter(n > 1)


# Identificar archivos presentes en la auditoría
# pero ausentes en la comparación.

archivos_sin_comparacion <- claves_auditoria %>%
  anti_join(
    claves_comparacion,
    by = c("universo", "sexo")
  )


# Identificar comparaciones sin archivo correspondiente
# en la auditoría.

comparaciones_sin_auditoria <- claves_comparacion %>%
  anti_join(
    claves_auditoria,
    by = c("universo", "sexo")
  )


cat("\nArchivos registrados en la auditoría:\n")
print(nrow(claves_auditoria))


cat("\nArchivos registrados en la comparación:\n")
print(nrow(claves_comparacion))


cat("\nClaves duplicadas en la auditoría:\n")
print(nrow(duplicados_auditoria))


cat("\nArchivos sin comparación:\n")
print(nrow(archivos_sin_comparacion))


cat("\nComparaciones sin archivo en la auditoría:\n")
print(nrow(comparaciones_sin_auditoria))


stopifnot(
  
  nrow(claves_auditoria) == 52,
  
  nrow(claves_comparacion) == 52,
  
  nrow(duplicados_auditoria) == 0,
  
  nrow(archivos_sin_comparacion) == 0,
  
  nrow(comparaciones_sin_auditoria) == 0,
  
  n_distinct(claves_auditoria$universo) == 26,
  
  n_distinct(claves_auditoria$sexo) == 2,
  
  n_distinct(
    paste(
      claves_auditoria$universo,
      claves_auditoria$sexo
    )
  ) == 52
)


cat("\nCORRESPONDENCIA DE LOS 52 ARCHIVOS VERIFICADA.\n")


# ------------------------------------------------------------
# 8.2. RESUMIR LOS TOTALES ORIGINALES Y PROCESADOS
#       POR UNIVERSO Y SEXO
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("8.2. RESUMEN DE TOTALES POR UNIVERSO Y SEXO\n")
cat("============================================================\n")


# Función para evitar que los valores faltantes se conviertan
# silenciosamente en cero durante la suma.

sumar_completo <- function(x) {
  
  if (anyNA(x)) {
    
    NA_real_
    
  } else {
    
    sum(x)
    
  }
}


totales_validacion_archivo <- comparacion_totales_52 %>%
  group_by(
    universo,
    sexo
  ) %>%
  summarise(
    
    combinaciones_comparadas = n(),
    
    combinaciones_sin_original = sum(
      falta_original |
        is.na(total_redatam)
    ),
    
    combinaciones_sin_procesado = sum(
      falta_procesado |
        is.na(suma_edades_procesadas)
    ),
    
    combinaciones_con_diferencias = sum(
      is.na(diferencia_procesada) |
        diferencia_procesada != 0
    ),
    
    total_original = sumar_completo(
      total_redatam
    ),
    
    total_procesado = sumar_completo(
      suma_edades_procesadas
    ),
    
    diferencia_original_procesado = if (
      anyNA(total_redatam) ||
      anyNA(suma_edades_procesadas)
    ) {
      
      NA_real_
      
    } else {
      
      sum(suma_edades_procesadas) -
        sum(total_redatam)
      
    },
    
    .groups = "drop"
  )


cat("\nDimensiones del resumen:\n")
print(dim(totales_validacion_archivo))


cat("\nPrimeros registros del resumen:\n")

print(
  totales_validacion_archivo %>%
    slice_head(n = 10)
)

# ============================================================
# 8.3. INCORPORAR LOS TOTALES DE LOS ARCHIVOS PROCESADOS
# ============================================================

# OBJETIVO:
#
# Recuperar los totales de defunciones de cada uno de los
# 52 archivos procesados individualmente.
#
# Los totales se calculan sumando únicamente las categorías
# de edad disponibles.
#
# Esto es importante porque en el archivo 2012 Mujeres
# la categoría "Edad ignorada" no está disponible y sus
# valores NA no deben convertirse en cero.
#
# Posteriormente, se comparan estos totales con:
#
# 1. Los totales originales recuperados de REDATAM.
# 2. Los totales de la base integrada.
#
# La identificación de cada archivo se realiza mediante
# las variables universo y sexo contenidas en cada tibble.


# ------------------------------------------------------------
# 8.3.1. Calcular los totales de los 52 archivos
# ------------------------------------------------------------

totales_archivos_procesados <- purrr::map_dfr(
  
  seq_along(bases_archivos),
  
  function(i) {
    
    datos_archivo <- bases_archivos[[i]]
    
    
    # Verificar que cada archivo contiene un único universo
    # y un único sexo.
    
    stopifnot(
      dplyr::n_distinct(datos_archivo$universo) == 1L,
      dplyr::n_distinct(datos_archivo$sexo) == 1L
    )
    
    
    # Verificar que no existen valores faltantes en las
    # categorías de edad disponibles.
    
    stopifnot(
      !anyNA(
        datos_archivo$defunciones[
          datos_archivo$categoria_disponible
        ]
      )
    )
    
    
    # Calcular el total utilizando exclusivamente las
    # categorías de edad disponibles.
    
    datos_archivo %>%
      summarise(
        
        universo = first(universo),
        
        sexo = first(sexo),
        
        filas_procesadas = n(),
        
        categorias_no_disponibles = sum(
          !categoria_disponible
        ),
        
        total_archivo_procesado = sum(
          defunciones[categoria_disponible]
        ),
        
        .groups = "drop"
      )
    
  }
)


# ------------------------------------------------------------
# 8.3.2. Comprobar la cobertura de los 52 archivos
# ------------------------------------------------------------

stopifnot(
  
  nrow(totales_archivos_procesados) == 52L,
  
  dplyr::n_distinct(
    totales_archivos_procesados$universo
  ) == 26L,
  
  dplyr::n_distinct(
    totales_archivos_procesados$sexo
  ) == 2L,
  
  nrow(
    totales_archivos_procesados %>%
      count(universo, sexo) %>%
      filter(n > 1)
  ) == 0L,
  
  nrow(
    anti_join(
      claves_auditoria,
      totales_archivos_procesados %>%
        select(universo, sexo),
      by = c("universo", "sexo")
    )
  ) == 0L
)


# ------------------------------------------------------------
# 8.3.3. Incorporar los totales a la validación por archivo
# ------------------------------------------------------------

validacion_final_por_archivo <- totales_validacion_archivo %>%
  
  left_join(
    
    totales_archivos_procesados,
    
    by = c("universo", "sexo")
    
  ) %>%
  
  arrange(
    universo,
    sexo
  )


# ------------------------------------------------------------
# 8.3.4. Identificar diferencias entre los tres totales
# ------------------------------------------------------------

validacion_final_por_archivo <- validacion_final_por_archivo %>%
  
  mutate(
    
    diferencia_original_archivo = 
      total_original - total_archivo_procesado,
    
    diferencia_integrado_archivo = 
      total_procesado - total_archivo_procesado,
    
    coincide_original_archivo = 
      total_original == total_archivo_procesado,
    
    coincide_integrado_archivo = 
      total_procesado == total_archivo_procesado
    
  )


# ------------------------------------------------------------
# 8.3.5. Mostrar la validación final por archivo
# ------------------------------------------------------------

print(
  validacion_final_por_archivo,
  n = 52
)


# ------------------------------------------------------------
# 8.3.6. Identificar archivos con diferencias
# ------------------------------------------------------------

archivos_con_diferencias_finales <- 
  validacion_final_por_archivo %>%
  
  filter(
    is.na(total_original) |
      is.na(total_procesado) |
      is.na(total_archivo_procesado) |
      !coincide_original_archivo |
      !coincide_integrado_archivo
  )


print(archivos_con_diferencias_finales)


# ------------------------------------------------------------
# 8.3.7. Validar la correspondencia de los totales
# ------------------------------------------------------------

stopifnot(
  
  nrow(validacion_final_por_archivo) == 52L,
  
  !anyNA(
    validacion_final_por_archivo$total_original
  ),
  
  !anyNA(
    validacion_final_por_archivo$total_procesado
  ),
  
  !anyNA(
    validacion_final_por_archivo$total_archivo_procesado
  ),
  
  nrow(archivos_con_diferencias_finales) == 0L
  
)


cat(
  "\nVALIDACIÓN DE LOS TOTALES POR ARCHIVO SUPERADA.\n"
)
# ------------------------------------------------------------
# 8.4. CONSULTAR LOS RESULTADOS DE LOS 52 ARCHIVOS
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("8.4. RESULTADOS FINALES POR ARCHIVO\n")
cat("============================================================\n")


print(
  validacion_final_por_archivo,
  n = Inf,
  width = Inf
)


# ============================================================
# 8.5. ARCHIVOS QUE REQUIEREN REVISIÓN
# ============================================================

# OBJETIVO:
#
# Identificar los archivos que presentan alguna diferencia
# entre los totales originales de REDATAM, los archivos
# procesados individualmente y la base integrada.
#
# Se utiliza el objeto archivos_con_diferencias_finales,
# construido previamente en la sección 8.3.6.


cat(
  "\n============================================================\n"
)

cat(
  "8.5. ARCHIVOS QUE REQUIEREN REVISIÓN\n"
)

cat(
  "============================================================\n"
)


# Consultar los archivos que presentan diferencias.

archivos_revisar <- archivos_con_diferencias_finales


print(
  archivos_revisar,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------
# 8.5.1. Verificar el resultado
# ------------------------------------------------------------

stopifnot(
  nrow(archivos_revisar) == 0L
)


cat(
  "\nNO SE IDENTIFICARON ARCHIVOS QUE REQUIERAN REVISIÓN.\n"
)


# ------------------------------------------------------------
# 8.6. RESUMEN GENERAL DE LA VALIDACIÓN
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("8.6. RESUMEN GENERAL\n")
cat("============================================================\n")


resumen_final_archivos <- validacion_final_por_archivo %>%
  summarise(
    
    archivos_comparados = n(),
    
    archivos_correctos = sum(
      coincide_original_archivo &
        coincide_integrado_archivo,
      na.rm = TRUE
    ),
    
    archivos_con_diferencias = sum(
      !coalesce(
        coincide_original_archivo &
          coincide_integrado_archivo,
        FALSE
      )
    ),
    
    combinaciones_comparadas = sum(
      combinaciones_comparadas
    ),
    
    total_archivos_procesados = sumar_completo(
      total_archivo_procesado
    ),
    
    total_original_redatam = sumar_completo(
      total_original
    ),
    
    total_base_integrada = sumar_completo(
      total_procesado
    ),
    
    diferencia_total_archivo_original =
      sumar_completo(diferencia_original_archivo),
    
    diferencia_total_archivo_integrado =
      sumar_completo(diferencia_integrado_archivo),
    
    diferencia_total_original_integrado =
      sumar_completo(diferencia_original_procesado)
  )


print(
  resumen_final_archivos,
  width = Inf
)


# ------------------------------------------------------------
# 8.7. VALIDACIONES FINALES
# ------------------------------------------------------------

stopifnot(
  
  # Número de archivos y cobertura.
  
  nrow(validacion_final_por_archivo) == 52L,
  
  n_distinct(
    validacion_final_por_archivo$universo
  ) == 26L,
  
  n_distinct(
    validacion_final_por_archivo$sexo
  ) == 2L,
  
  n_distinct(
    paste(
      validacion_final_por_archivo$universo,
      validacion_final_por_archivo$sexo
    )
  ) == 52L,
  
  
  # Integridad de las comparaciones.
  
  all(
    !is.na(
      validacion_final_por_archivo$combinaciones_comparadas
    )
  ),
  
  all(
    validacion_final_por_archivo$combinaciones_comparadas > 0
  ),
  
  all(
    validacion_final_por_archivo$combinaciones_sin_original == 0
  ),
  
  all(
    validacion_final_por_archivo$combinaciones_sin_procesado == 0
  ),
  
  all(
    validacion_final_por_archivo$combinaciones_con_diferencias == 0
  ),
  
  
  # Ausencia de resultados faltantes.
  
  all(
    !is.na(
      validacion_final_por_archivo$total_archivo_procesado
    )
  ),
  
  all(
    !is.na(
      validacion_final_por_archivo$total_original
    )
  ),
  
  all(
    !is.na(
      validacion_final_por_archivo$total_procesado
    )
  ),
  
  
  # Igualdad de los tres totales por archivo.
  
  all(
    validacion_final_por_archivo$diferencia_original_archivo == 0
  ),
  
  all(
    validacion_final_por_archivo$diferencia_integrado_archivo == 0
  ),
  
  all(
    validacion_final_por_archivo$diferencia_original_procesado == 0
  ),
  
  
  # Todos los archivos deben estar correctamente validados.
  
  nrow(archivos_revisar) == 0L,
  
  all(
    validacion_final_por_archivo$coincide_original_archivo
  ),
  
  all(
    validacion_final_por_archivo$coincide_integrado_archivo
  ),
  
  
  # Igualdad de los totales generales.
  
  resumen_final_archivos$archivos_comparados == 52L,
  
  resumen_final_archivos$archivos_correctos == 52L,
  
  resumen_final_archivos$archivos_con_diferencias == 0L,
  
  resumen_final_archivos$total_archivos_procesados ==
    resumen_final_archivos$total_original_redatam,
  
  resumen_final_archivos$total_original_redatam ==
    resumen_final_archivos$total_base_integrada,
  
  resumen_final_archivos$diferencia_total_archivo_original == 0,
  
  resumen_final_archivos$diferencia_total_archivo_integrado == 0,
  
  resumen_final_archivos$diferencia_total_original_integrado == 0
)


cat("\n============================================================\n")
cat("SECCIÓN 8 COMPLETADA.\n")
cat("LOS 52 ARCHIVOS PRESENTAN TOTALES CONSISTENTES.\n")
cat("LAS TRES FUENTES DE TOTALES COINCIDEN.\n")
cat("============================================================\n")





# ============================================================
# 9. VALIDACIÓN DE LA ESTRUCTURA TEMPORAL, TERRITORIAL Y ETARIA
# ============================================================

# OBJETIVO:
#
# Evaluar la cobertura y consistencia estructural de la base
# integrada de defunciones REDATAM.
#
# Se examinarán:
#
# 1. Los 26 universos (2000-2025).
# 2. Los dos sexos.
# 3. Las siete provincias.
# 4. Las 23 categorías de edad.
# 5. La cobertura territorial por universo y sexo.
# 6. La integridad de las categorías de edad en cada grupo.
# 7. La disponibilidad de las categorías de edad.
# 8. La consistencia de los totales agregados.
#
# IMPORTANTE:
#
# universo identifica el año del archivo de origen.
# No equivale al año de declaración ni al año de defunción.
#
# No se exige que todas las combinaciones posibles de años
# de declaración y defunción estén presentes.
#
# La categoría Edad ignorada está estructuralmente presente
# en los 52 archivos.
#
# Sin embargo, sus valores no están disponibles en el archivo
# correspondiente a 2012 para mujeres.
#
# Los totales se calculan utilizando exclusivamente las
# categorías disponibles.
#
# Los recuentos son acumulados entre los universos REDATAM.
# No representan necesariamente defunciones únicas.
#
# ============================================================


# ------------------------------------------------------------
# 9.1. DEFINIR LAS DIMENSIONES ESPERADAS
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("9.1. DIMENSIONES ESPERADAS\n")
cat("============================================================\n")

universos_esperados <- 2000:2025

sexos_esperados <- c(
  "Hombre",
  "Mujer"
)

provincias_esperadas <- tibble(
  provincia_id = 1:7,
  provincia = c(
    "San José",
    "Alajuela",
    "Cartago",
    "Heredia",
    "Guanacaste",
    "Puntarenas",
    "Limón"
  )
)

cat("\nUniversos esperados:\n")
print(universos_esperados)

cat("\nSexos esperados:\n")
print(sexos_esperados)

cat("\nProvincias esperadas:\n")
print(provincias_esperadas)

cat("\nCategorías de edad esperadas:\n")
print(length(edades_esperadas))


# ------------------------------------------------------------
# 9.2. COMPROBAR LA COBERTURA DE UNIVERSOS Y SEXOS
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("9.2. COBERTURA DE UNIVERSOS Y SEXOS\n")
cat("============================================================\n")

# Identificar las combinaciones observadas.

cobertura_universos_sexos <- datos_completos %>%
  distinct(
    universo,
    sexo
  ) %>%
  arrange(
    universo,
    sexo
  )

cat("\nCombinaciones observadas:\n")

print(
  cobertura_universos_sexos,
  n = Inf
)

# Construir todas las combinaciones esperadas.

combinaciones_universo_sexo <- tidyr::crossing(
  universo = universos_esperados,
  sexo = sexos_esperados
) %>%
  left_join(
    cobertura_universos_sexos %>%
      mutate(presente = TRUE),
    by = c("universo", "sexo")
  ) %>%
  mutate(
    presente = tidyr::replace_na(
      presente,
      FALSE
    )
  )

# Identificar combinaciones faltantes.

faltantes_universo_sexo <- combinaciones_universo_sexo %>%
  filter(!presente)

cat("\nCombinaciones universo-sexo faltantes:\n")

print(
  faltantes_universo_sexo,
  n = Inf
)

# Identificar universos no esperados.

universos_presentes <- datos_completos %>%
  distinct(universo) %>%
  arrange(universo)

universos_no_esperados <- universos_presentes %>%
  filter(!universo %in% universos_esperados)

cat("\nUniversos no esperados:\n")

print(
  universos_no_esperados,
  n = Inf
)

# Identificar sexos no esperados.

sexos_presentes <- datos_completos %>%
  distinct(sexo) %>%
  arrange(sexo)

sexos_no_esperados <- sexos_presentes %>%
  filter(!sexo %in% sexos_esperados)

cat("\nSexos no esperados:\n")

print(
  sexos_no_esperados,
  n = Inf
)

# Resumen de cobertura.

resumen_cobertura_universos <- combinaciones_universo_sexo %>%
  summarise(
    combinaciones_esperadas = n(),
    combinaciones_presentes = sum(presente),
    combinaciones_faltantes = sum(!presente)
  )

cat("\nResumen de cobertura:\n")

print(
  resumen_cobertura_universos
)


# ------------------------------------------------------------
# 9.3. COMPROBAR LA COBERTURA DE LAS SIETE PROVINCIAS
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("9.3. COBERTURA DE LAS PROVINCIAS\n")
cat("============================================================\n")

# Identificar las provincias observadas.

cobertura_provincias <- datos_completos %>%
  distinct(
    provincia_id,
    provincia
  ) %>%
  arrange(
    provincia_id
  )

cat("\nProvincias observadas:\n")

print(
  cobertura_provincias,
  n = Inf
)

# Identificar provincias esperadas que no aparecen.

provincias_faltantes <- provincias_esperadas %>%
  anti_join(
    cobertura_provincias,
    by = c("provincia_id", "provincia")
  )

cat("\nProvincias faltantes:\n")

print(
  provincias_faltantes,
  n = Inf
)

# Identificar provincias observadas que no corresponden
# a la clasificación territorial esperada.
#
# Esta comprobación también detecta nombres incorrectos
# asociados a un ID provincial.

provincias_no_esperadas <- cobertura_provincias %>%
  anti_join(
    provincias_esperadas,
    by = c("provincia_id", "provincia")
  )

cat("\nProvincias no esperadas o identificaciones incorrectas:\n")

print(
  provincias_no_esperadas,
  n = Inf
)


# ------------------------------------------------------------
# 9.4. COMPROBAR LA COBERTURA TERRITORIAL
#      POR UNIVERSO Y SEXO
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("9.4. COBERTURA TERRITORIAL POR UNIVERSO Y SEXO\n")
cat("============================================================\n")

# Construir las 364 combinaciones esperadas:
#
# 26 universos × 2 sexos × 7 provincias.

combinaciones_territoriales <- tidyr::crossing(
  universo = universos_esperados,
  sexo = sexos_esperados,
  provincias_esperadas
)

# Identificar las combinaciones realmente observadas.

cobertura_territorial <- datos_completos %>%
  distinct(
    universo,
    sexo,
    provincia_id,
    provincia
  ) %>%
  mutate(
    presente = TRUE
  )

# Comparar combinaciones esperadas y observadas.

validacion_territorial <- combinaciones_territoriales %>%
  left_join(
    cobertura_territorial,
    by = c(
      "universo",
      "sexo",
      "provincia_id",
      "provincia"
    )
  ) %>%
  mutate(
    presente = tidyr::replace_na(
      presente,
      FALSE
    )
  )

# Identificar combinaciones territoriales faltantes.

combinaciones_territoriales_faltantes <-
  validacion_territorial %>%
  filter(!presente)

cat("\nCombinaciones territoriales faltantes:\n")

print(
  combinaciones_territoriales_faltantes,
  n = Inf
)

# Resumen de cobertura territorial.

resumen_cobertura_territorial <- validacion_territorial %>%
  summarise(
    combinaciones_esperadas = n(),
    combinaciones_presentes = sum(presente),
    combinaciones_faltantes = sum(!presente)
  )

cat("\nResumen de cobertura territorial:\n")

print(
  resumen_cobertura_territorial
)


# ------------------------------------------------------------
# 9.5. COMPROBAR LAS CATEGORÍAS DE EDAD PRESENTES
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("9.5. COBERTURA DE LAS CATEGORÍAS DE EDAD\n")
cat("============================================================\n")

# Identificar las categorías observadas.

categorias_edad_presentes <- datos_completos %>%
  distinct(
    edad_quinquenal
  ) %>%
  arrange(
    match(
      edad_quinquenal,
      edades_esperadas
    )
  )

cat("\nCategorías de edad observadas:\n")

print(
  categorias_edad_presentes,
  n = Inf
)

# Identificar categorías esperadas que no aparecen.

categorias_edad_faltantes <- tibble(
  edad_quinquenal = edades_esperadas
) %>%
  anti_join(
    categorias_edad_presentes,
    by = "edad_quinquenal"
  )

cat("\nCategorías de edad faltantes:\n")

print(
  categorias_edad_faltantes,
  n = Inf
)

# Identificar categorías observadas que no están
# incluidas en la clasificación esperada.

categorias_edad_no_esperadas <- categorias_edad_presentes %>%
  filter(
    !edad_quinquenal %in% edades_esperadas
  )

cat("\nCategorías de edad no esperadas:\n")

print(
  categorias_edad_no_esperadas,
  n = Inf
)


# ------------------------------------------------------------
# 9.6. COMPROBAR LAS 23 CATEGORÍAS DE EDAD
#      EN CADA COMBINACIÓN
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("9.6. INTEGRIDAD DE LAS CATEGORÍAS DE EDAD\n")
cat("============================================================\n")

# Cada combinación observada de:
#
# universo + sexo + provincia + año de declaración +
# año de defunción
#
# debe contener las 23 categorías de edad.
#
# Se comprueba tanto el número de filas como el número
# de categorías distintas.
#
# Esto permite detectar categorías ausentes y filas
# duplicadas dentro de una combinación.

categorias_por_combinacion <- datos_completos %>%
  group_by(
    universo,
    sexo,
    provincia_id,
    provincia,
    anio_declaracion,
    anio_defuncion
  ) %>%
  summarise(
    filas_presentes = n(),
    categorias_presentes = n_distinct(
      edad_quinquenal
    ),
    .groups = "drop"
  ) %>%
  mutate(
    categorias_esperadas = length(
      edades_esperadas
    ),
    estado = if_else(
      filas_presentes == categorias_esperadas &
        categorias_presentes == categorias_esperadas,
      "Correcto",
      "Revisar"
    )
  )

# Identificar combinaciones incompletas.

combinaciones_edad_incompletas <- categorias_por_combinacion %>%
  filter(
    estado != "Correcto"
  )

cat("\nCombinaciones con categorías de edad incompletas:\n")

print(
  combinaciones_edad_incompletas,
  n = Inf
)

# Resumen de integridad de las categorías de edad.

resumen_categorias_edad <- categorias_por_combinacion %>%
  summarise(
    combinaciones_evaluadas = n(),
    combinaciones_correctas = sum(
      estado == "Correcto"
    ),
    combinaciones_incompletas = sum(
      estado != "Correcto"
    ),
    minimo_filas = min(
      filas_presentes
    ),
    maximo_filas = max(
      filas_presentes
    ),
    minimo_categorias = min(
      categorias_presentes
    ),
    maximo_categorias = max(
      categorias_presentes
    )
  )

cat("\nResumen de integridad de categorías de edad:\n")

print(
  resumen_categorias_edad
)


# ------------------------------------------------------------
# 9.7. CALCULAR LOS RECUENTOS POR UNIVERSO,
#      PROVINCIA Y SEXO
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("9.7. RECUENTOS POR UNIVERSO, PROVINCIA Y SEXO\n")
cat("============================================================\n")

# Se suman exclusivamente las categorías disponibles.
#
# Los valores no disponibles de Edad ignorada en 2012
# para mujeres no se interpretan como ceros.

recuentos_por_universo_provincia_sexo <- datos_completos %>%
  group_by(
    universo,
    provincia_id,
    provincia,
    sexo
  ) %>%
  summarise(
    total_defunciones = sum(
      defunciones[categoria_disponible]
    ),
    .groups = "drop"
  ) %>%
  arrange(
    universo,
    provincia_id,
    sexo
  )

cat("\nTotales por universo, provincia y sexo:\n")

print(
  recuentos_por_universo_provincia_sexo,
  n = Inf
)


# ------------------------------------------------------------
# 9.8. CALCULAR LOS RECUENTOS POR UNIVERSO Y SEXO
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("9.8. RECUENTOS POR UNIVERSO Y SEXO\n")
cat("============================================================\n")

recuentos_por_universo_sexo <- datos_completos %>%
  group_by(
    universo,
    sexo
  ) %>%
  summarise(
    total_defunciones = sum(
      defunciones[categoria_disponible]
    ),
    .groups = "drop"
  ) %>%
  arrange(
    universo,
    sexo
  )

print(
  recuentos_por_universo_sexo,
  n = Inf
)


# ------------------------------------------------------------
# 9.9. CALCULAR LOS RECUENTOS POR PROVINCIA Y SEXO
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("9.9. RECUENTOS POR PROVINCIA Y SEXO\n")
cat("============================================================\n")

recuentos_por_provincia_sexo <- datos_completos %>%
  group_by(
    provincia_id,
    provincia,
    sexo
  ) %>%
  summarise(
    total_defunciones = sum(
      defunciones[categoria_disponible]
    ),
    .groups = "drop"
  ) %>%
  arrange(
    provincia_id,
    sexo
  )

print(
  recuentos_por_provincia_sexo,
  n = Inf
)


# ------------------------------------------------------------
# 9.10. COMPROBAR LA CONSISTENCIA DE LOS TOTALES AGREGADOS
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("9.10. VALIDACIÓN DE LOS TOTALES AGREGADOS\n")
cat("============================================================\n")

# Total general calculado exclusivamente con categorías
# disponibles.

total_general <- sum(
  datos_completos$defunciones[
    datos_completos$categoria_disponible
  ]
)

# Suma de los recuentos agregados por provincia y sexo.

total_por_provincia <- sum(
  recuentos_por_provincia_sexo$total_defunciones
)

# Suma de los recuentos agregados por universo y sexo.

total_por_universo_sexo <- sum(
  recuentos_por_universo_sexo$total_defunciones
)

validacion_totales_estructura <- tibble(
  total_general = total_general,
  total_por_provincia = total_por_provincia,
  total_por_universo_sexo = total_por_universo_sexo,
  diferencia_provincias =
    total_por_provincia - total_general,
  diferencia_universo_sexo =
    total_por_universo_sexo - total_general
)

print(
  validacion_totales_estructura,
  width = Inf
)


# ------------------------------------------------------------
# 9.11. RESUMEN GENERAL Y VALIDACIONES FINALES
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("9.11. RESUMEN GENERAL DE LA VALIDACIÓN ESTRUCTURAL\n")
cat("============================================================\n")

# Identificar los registros con categorías no disponibles.

datos_no_disponibles <- datos_completos %>%
  filter(
    !categoria_disponible
  )

# Identificar faltantes en categorías que deberían estar disponibles.

faltantes_en_categorias_disponibles <- datos_completos %>%
  filter(
    categoria_disponible,
    is.na(defunciones)
  )

# Identificar recuentos negativos.

recuentos_negativos <- datos_completos %>%
  filter(
    categoria_disponible,
    defunciones < 0
  )

resumen_validacion_estructura <- tibble(
  universos_presentes = n_distinct(
    datos_completos$universo
  ),
  sexos_presentes = n_distinct(
    datos_completos$sexo
  ),
  provincias_presentes = n_distinct(
    datos_completos$provincia_id
  ),
  categorias_edad_presentes = n_distinct(
    datos_completos$edad_quinquenal
  ),
  combinaciones_universo_sexo_faltantes =
    nrow(faltantes_universo_sexo),
  universos_no_esperados =
    nrow(universos_no_esperados),
  sexos_no_esperados =
    nrow(sexos_no_esperados),
  provincias_faltantes =
    nrow(provincias_faltantes),
  provincias_no_esperadas =
    nrow(provincias_no_esperadas),
  combinaciones_territoriales_faltantes =
    nrow(combinaciones_territoriales_faltantes),
  categorias_edad_faltantes =
    nrow(categorias_edad_faltantes),
  categorias_edad_no_esperadas =
    nrow(categorias_edad_no_esperadas),
  combinaciones_edad_incompletas =
    nrow(combinaciones_edad_incompletas),
  categorias_no_disponibles =
    nrow(datos_no_disponibles),
  faltantes_en_categorias_disponibles =
    nrow(faltantes_en_categorias_disponibles),
  recuentos_negativos =
    nrow(recuentos_negativos),
  total_recuentos_disponibles =
    total_general
)

print(
  resumen_validacion_estructura,
  width = Inf
)


# ------------------------------------------------------------
# 9.12. VALIDACIONES FINALES
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("9.12. VALIDACIONES FINALES\n")
cat("============================================================\n")

stopifnot(
  
  # ----------------------------------------------------------
  # Integridad de las variables principales.
  # ----------------------------------------------------------
  
  !anyNA(datos_completos$universo),
  
  !anyNA(datos_completos$sexo),
  
  !anyNA(datos_completos$provincia_id),
  
  !anyNA(datos_completos$provincia),
  
  !anyNA(datos_completos$edad_quinquenal),
  
  !anyNA(datos_completos$categoria_disponible),
  
  
  # ----------------------------------------------------------
  # Dimensiones generales.
  # ----------------------------------------------------------
  
  n_distinct(datos_completos$universo) == 26,
  
  n_distinct(datos_completos$sexo) == 2,
  
  n_distinct(datos_completos$provincia_id) == 7,
  
  n_distinct(datos_completos$edad_quinquenal) == 23,
  
  
  # ----------------------------------------------------------
  # Cobertura de universos y sexos.
  # ----------------------------------------------------------
  
  nrow(faltantes_universo_sexo) == 0,
  
  nrow(cobertura_universos_sexos) == 52,
  
  nrow(universos_no_esperados) == 0,
  
  nrow(sexos_no_esperados) == 0,
  
  
  # ----------------------------------------------------------
  # Cobertura provincial.
  # ----------------------------------------------------------
  
  nrow(provincias_faltantes) == 0,
  
  nrow(provincias_no_esperadas) == 0,
  
  nrow(cobertura_provincias) == 7,
  
  
  # ----------------------------------------------------------
  # Cobertura territorial.
  # ----------------------------------------------------------
  
  nrow(combinaciones_territoriales) == 364,
  
  nrow(combinaciones_territoriales_faltantes) == 0,
  
  
  # ----------------------------------------------------------
  # Categorías de edad.
  # ----------------------------------------------------------
  
  nrow(categorias_edad_faltantes) == 0,
  
  nrow(categorias_edad_no_esperadas) == 0,
  
  nrow(combinaciones_edad_incompletas) == 0,
  
  all(
    categorias_por_combinacion$filas_presentes ==
      length(edades_esperadas)
  ),
  
  all(
    categorias_por_combinacion$categorias_presentes ==
      length(edades_esperadas)
  ),
  
  
  # ----------------------------------------------------------
  # Disponibilidad de las categorías de edad.
  # ----------------------------------------------------------
  
  nrow(datos_no_disponibles) == 42,
  
  all(
    datos_no_disponibles$universo == 2012L
  ),
  
  all(
    datos_no_disponibles$sexo == "Mujer"
  ),
  
  all(
    datos_no_disponibles$edad_quinquenal ==
      "Edad ignorada"
  ),
  
  nrow(faltantes_en_categorias_disponibles) == 0,
  
  nrow(recuentos_negativos) == 0,
  
  
  # ----------------------------------------------------------
  # Consistencia de los totales.
  # ----------------------------------------------------------
  
  total_general == total_por_provincia,
  
  total_general == total_por_universo_sexo,
  
  total_general == resumen_final_archivos$total_base_integrada
  
)

cat("\n============================================================\n")
cat("SECCIÓN 9 COMPLETADA.\n")
cat("LA VALIDACIÓN ESTRUCTURAL FUE SUPERADA.\n")
cat("============================================================\n")



# ============================================================
# 10. VALIDACIÓN DE LOS AÑOS DE DECLARACIÓN Y DEFUNCIÓN
# ============================================================

# OBJETIVO:
#
# Examinar los intervalos temporales de las defunciones
# y cuantificar la diferencia entre el año de ocurrencia
# y el año de declaración.
#
# Se examinarán:
#
# 1. Los años mínimos y máximos de declaración y defunción.
# 2. La diferencia temporal entre ambas variables.
# 3. La distribución de los retrasos.
# 4. El volumen de recuentos según su condición temporal.
# 5. Las anomalías cronológicas.
#
# IMPORTANTE:
#
# universo identifica el año del archivo REDATAM.
# No debe confundirse con el año de ocurrencia o declaración.
#
# retraso_declaracion =
#     anio_declaracion - anio_defuncion
#
# Interpretación:
#
# 0  = declaración en el mismo año de la defunción.
# >0 = declaración posterior al año de la defunción.
# <0 = declaración anterior al año de la defunción.
#
# Los recuentos son acumulados entre los universos disponibles.
# No representan necesariamente defunciones únicas.
#
# Los registros de categorías no disponibles no se interpretan
# como ceros ni se utilizan en los totales.
#
# ============================================================


# ------------------------------------------------------------
# 10.1. IDENTIFICAR LOS AÑOS PRESENTES EN AMBAS VARIABLES
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("10.1. RANGOS TEMPORALES DE DECLARACIÓN Y DEFUNCIÓN\n")
cat("============================================================\n")

# Comprobar previamente que los años estén completos
# y tengan formato numérico.

stopifnot(
  
  is.numeric(datos_completos$anio_declaracion),
  
  is.numeric(datos_completos$anio_defuncion),
  
  !anyNA(datos_completos$anio_declaracion),
  
  !anyNA(datos_completos$anio_defuncion)
  
)

rango_temporal <- datos_completos %>%
  summarise(
    minimo_anio_declaracion = min(
      anio_declaracion
    ),
    maximo_anio_declaracion = max(
      anio_declaracion
    ),
    minimo_anio_defuncion = min(
      anio_defuncion
    ),
    maximo_anio_defuncion = max(
      anio_defuncion
    )
  )

print(
  rango_temporal,
  width = Inf
)


# ------------------------------------------------------------
# 10.2. CALCULAR LA DIFERENCIA ENTRE DECLARACIÓN Y DEFUNCIÓN
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("10.2. CALCULAR EL RETRASO DE DECLARACIÓN\n")
cat("============================================================\n")

# Se incorpora la variable a la base integrada.
#
# Esta variable se utilizará en los análisis posteriores.

datos_completos <- datos_completos %>%
  mutate(
    retraso_declaracion =
      anio_declaracion - anio_defuncion
  )

cat("\nResumen de la nueva variable:\n")

summary(
  datos_completos$retraso_declaracion
)

cat("\nValores distintos del retraso:\n")

print(
  sort(
    unique(datos_completos$retraso_declaracion)
  )
)


# ------------------------------------------------------------
# 10.3. DISTRIBUCIÓN DE LAS DIFERENCIAS TEMPORALES
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("10.3. DISTRIBUCIÓN DE LOS RETRASOS DE DECLARACIÓN\n")
cat("============================================================\n")

distribucion_retrasos <- datos_completos %>%
  group_by(
    retraso_declaracion
  ) %>%
  summarise(
    filas = n(),
    total_defunciones = sum(
      defunciones[categoria_disponible]
    ),
    .groups = "drop"
  ) %>%
  arrange(
    retraso_declaracion
  )

cat("\nDistribución de los retrasos:\n")

print(
  distribucion_retrasos,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------
# 10.4. RESUMEN DE LOS RETRASOS CON PONDERACIÓN
#       POR RECUENTOS DE DEFUNCIONES
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("10.4. RESUMEN DE RECUENTOS SEGÚN RETRASO\n")
cat("============================================================\n")

resumen_retrasos <- datos_completos %>%
  summarise(
    
    # Total de recuentos disponibles.
    
    total_defunciones = sum(
      defunciones[categoria_disponible]
    ),
    
    # Recuentos correspondientes a declaraciones
    # realizadas en el mismo año de la defunción.
    
    defunciones_sin_retraso = sum(
      defunciones[
        categoria_disponible &
          retraso_declaracion == 0
      ]
    ),
    
    # Recuentos correspondientes a declaraciones
    # posteriores al año de la defunción.
    
    defunciones_declaradas_tarde = sum(
      defunciones[
        categoria_disponible &
          retraso_declaracion > 0
      ]
    ),
    
    # Recuentos correspondientes a anomalías
    # cronológicas: declaración anterior a la defunción.
    
    defunciones_con_anomalia_temporal = sum(
      defunciones[
        categoria_disponible &
          retraso_declaracion < 0
      ]
    )
    
  ) %>%
  mutate(
    
    porcentaje_sin_retraso =
      100 * defunciones_sin_retraso / total_defunciones,
    
    porcentaje_declaradas_tarde =
      100 * defunciones_declaradas_tarde / total_defunciones,
    
    porcentaje_anomalias =
      100 * defunciones_con_anomalia_temporal / total_defunciones
    
  )

cat("\nResumen ponderado por recuentos:\n")

print(
  resumen_retrasos,
  width = Inf
)


# ------------------------------------------------------------
# 10.5. IDENTIFICAR LAS COMBINACIONES CON RETRASOS NEGATIVOS
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("10.5. ANOMALÍAS CRONOLÓGICAS\n")
cat("============================================================\n")

# Se identifican las combinaciones en las que el año
# de declaración es anterior al año de defunción.
#
# Se utilizan exclusivamente las categorías disponibles.
#
# Estas observaciones requieren investigación.
# No se corrigen ni eliminan automáticamente.

anomalias_temporales <- datos_completos %>%
  filter(
    retraso_declaracion < 0,
    categoria_disponible
  ) %>%
  group_by(
    universo,
    sexo,
    provincia_id,
    provincia,
    anio_declaracion,
    anio_defuncion,
    retraso_declaracion
  ) %>%
  summarise(
    filas = n(),
    total_defunciones = sum(
      defunciones
    ),
    .groups = "drop"
  ) %>%
  arrange(
    universo,
    sexo,
    provincia_id
  )

cat("\nCombinaciones con anomalías cronológicas:\n")

print(
  anomalias_temporales,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------
# 10.6. VALIDACIONES FINALES DE LA SECCIÓN
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("10.6. VALIDACIONES FINALES\n")
cat("============================================================\n")


# ------------------------------------------------------------
# 10.6.1. CONSTRUIR RESUMEN DE VALIDACIÓN
# ------------------------------------------------------------

validacion_seccion_10 <- tibble(
  
  total_base = sum(
    datos_completos$defunciones[
      datos_completos$categoria_disponible
    ]
  ),
  
  total_distribucion_retrasos = sum(
    distribucion_retrasos$total_defunciones
  ),
  
  total_resumen_retrasos =
    resumen_retrasos$total_defunciones,
  
  defunciones_sin_retraso =
    resumen_retrasos$defunciones_sin_retraso,
  
  defunciones_declaradas_tarde =
    resumen_retrasos$defunciones_declaradas_tarde,
  
  defunciones_con_anomalia =
    resumen_retrasos$defunciones_con_anomalia_temporal,
  
  filas_con_anomalia = sum(
    datos_completos$retraso_declaracion < 0 &
      datos_completos$categoria_disponible
  ),
  
  combinaciones_anomalas = nrow(
    anomalias_temporales
  )
  
)

cat("\nResumen de validación:\n")

print(
  validacion_seccion_10,
  width = Inf
)



# ------------------------------------------------------------
# 10.6.2. COMPROBAR INTEGRIDAD DE LOS CONTEOS
# ------------------------------------------------------------

# ----------------------------------------------------------
# Comprobar que todas las filas disponibles quedan
# clasificadas en una de las tres condiciones temporales.
# ----------------------------------------------------------

filas_temporales_disponibles <- sum(
  datos_completos$categoria_disponible
)

filas_temporales_clasificadas <- sum(
  datos_completos$categoria_disponible &
    (
      datos_completos$retraso_declaracion == 0 |
        datos_completos$retraso_declaracion > 0 |
        datos_completos$retraso_declaracion < 0
    )
)


# ----------------------------------------------------------
# Construir las comprobaciones finales.
# ----------------------------------------------------------

comprobaciones_seccion_10 <- c(
  
  filas_temporales_clasificadas =
    filas_temporales_clasificadas ==
    filas_temporales_disponibles,
  
  retrasos_sin_faltantes =
    !anyNA(datos_completos$retraso_declaracion),
  
  total_base_vs_distribucion =
    isTRUE(
      validacion_seccion_10$total_base ==
        validacion_seccion_10$total_distribucion_retrasos
    ),
  
  total_base_vs_resumen =
    isTRUE(
      validacion_seccion_10$total_base ==
        validacion_seccion_10$total_resumen_retrasos
    ),
  
  suma_categorias =
    isTRUE(
      resumen_retrasos$total_defunciones ==
        resumen_retrasos$defunciones_sin_retraso +
        resumen_retrasos$defunciones_declaradas_tarde +
        resumen_retrasos$defunciones_con_anomalia_temporal
    ),
  
  retrasos_anomalos_negativos =
    nrow(anomalias_temporales) > 0L &&
    all(anomalias_temporales$retraso_declaracion < 0),
  
  recuentos_anomalos_positivos =
    nrow(anomalias_temporales) > 0L &&
    all(anomalias_temporales$total_defunciones > 0),
  
  total_anomalias_vs_detalle =
    isTRUE(
      sum(anomalias_temporales$total_defunciones) ==
        resumen_retrasos$defunciones_con_anomalia_temporal
    ),
  
  filas_anomalas_vs_detalle =
    isTRUE(
      validacion_seccion_10$filas_con_anomalia ==
        sum(anomalias_temporales$filas)
    ),
  
  numero_combinaciones_anomalas =
    nrow(anomalias_temporales) == 1L,
  
  total_recuentos_anomalos =
    isTRUE(
      resumen_retrasos$defunciones_con_anomalia_temporal == 1
    ),
  
  identificacion_anomalia_conocida =
    nrow(anomalias_temporales) == 1L &&
    all(anomalias_temporales$universo == 2019L) &&
    all(anomalias_temporales$sexo == "Hombre") &&
    all(anomalias_temporales$provincia == "Guanacaste") &&
    all(anomalias_temporales$anio_declaracion == 2018L) &&
    all(anomalias_temporales$anio_defuncion == 2019L) &&
    all(anomalias_temporales$retraso_declaracion == -1L) &&
    all(anomalias_temporales$total_defunciones == 1)
  
)


# ----------------------------------------------------------
# Mostrar resultados.
# ----------------------------------------------------------

cat("\nComprobaciones de integridad de la sección 10:\n")

print(comprobaciones_seccion_10)

cat(
  "\nResultado general:",
  all(comprobaciones_seccion_10),
  "\n"
)

# ------------------------------------------------------------
# 10.6.3. MOSTRAR RESULTADOS DE LAS COMPROBACIONES
# ------------------------------------------------------------

cat("\nResultados de las comprobaciones:\n")

print(
  tibble(
    comprobacion = names(comprobaciones_seccion_10),
    resultado = unname(comprobaciones_seccion_10)
  ),
  n = Inf
)


# ------------------------------------------------------------
# 10.6.4. DETENER LA EJECUCIÓN SI ALGUNA COMPROBACIÓN FALLA
# ------------------------------------------------------------

stopifnot(
  all(comprobaciones_seccion_10)
)


cat("\n============================================================\n")
cat("SECCIÓN 10 COMPLETADA.\n")
cat("TODAS LAS VALIDACIONES TEMPORALES FUERON SUPERADAS.\n")
cat("============================================================\n")





# ============================================================
# 11. DIAGNÓSTICO DE REPETICIONES ENTRE ARCHIVOS
# ============================================================

# OBJETIVO:
#
# Identificar combinaciones que aparecen en más de un universo
# (archivo de origen) y examinar sus valores de defunciones.
#
# Se evaluarán:
#
# 1. Combinaciones presentes en varios universos.
# 2. Coincidencia o diferencia de los valores disponibles.
# 3. Número de universos con valores disponibles.
# 4. Número de universos con valores no disponibles.
# 5. Distribución de valores positivos y ceros.
# 6. Combinaciones positivas en uno o varios universos.
#
# IMPORTANTE:
#
# universo identifica el año del archivo de origen.
#
# La repetición de una combinación agregada no demuestra
# que se trate de las mismas personas fallecidas.
#
# Los conteos entre universos no deben sumarse para estimar
# defunciones únicas.
#
# Los valores no disponibles no se interpretan como ceros
# ni como valores diferentes.
#
# Esta sección es exclusivamente diagnóstica.
#
# No vuelve a comprobar los formatos de los archivos Excel
# ni modifica la función leer_redatam().
#
# ============================================================


# ------------------------------------------------------------
# 11.1. IDENTIFICAR COMBINACIONES PRESENTES EN VARIOS UNIVERSOS
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("11.1. IDENTIFICACIÓN DE COMBINACIONES REPETIDAS\n")
cat("============================================================\n")


# Se excluye universo de las claves de agrupación.
#
# Se consideran:
#
# sexo + provincia + año de declaración + año de defunción
# + grupo de edad.
#
# Se mantienen todas las observaciones para identificar
# las combinaciones repetidas.
#
# Los valores no disponibles se excluyen exclusivamente
# de las comparaciones numéricas.


claves_repetidas_universos <- datos_completos %>%
  
  group_by(
    sexo,
    provincia_id,
    provincia,
    anio_declaracion,
    anio_defuncion,
    edad_quinquenal
  ) %>%
  
  summarise(
    
    # Número de universos en los que aparece la combinación.
    
    numero_universos = n_distinct(universo),
    
    # Años de los archivos en los que aparece.
    
    universos = paste(
      sort(unique(universo)),
      collapse = ", "
    ),
    
    # Número de universos con valores disponibles.
    
    universos_con_valor_disponible = n_distinct(
      universo[categoria_disponible]
    ),
    
    # Número de universos sin valores disponibles.
    
    universos_sin_valor_disponible = n_distinct(
      universo[!categoria_disponible]
    ),
    
    # Número de valores numéricos distintos,
    # considerando exclusivamente los disponibles.
    
    valores_distintos_disponibles = n_distinct(
      defunciones[categoria_disponible]
    ),
    
    # Conteo mínimo entre valores disponibles.
    
    minimo_defunciones = if (
      any(categoria_disponible)
    ) {
      min(defunciones[categoria_disponible])
    } else {
      NA_real_
    },
    
    # Conteo máximo entre valores disponibles.
    
    maximo_defunciones = if (
      any(categoria_disponible)
    ) {
      max(defunciones[categoria_disponible])
    } else {
      NA_real_
    },
    
    .groups = "drop"
  ) %>%
  
  filter(
    numero_universos > 1
  )


cat("\nNúmero de combinaciones presentes en varios universos:\n")

print(
  nrow(claves_repetidas_universos)
)


cat("\nPrimeras 30 combinaciones repetidas:\n")

print(
  claves_repetidas_universos,
  n = 30,
  width = Inf
)


# ------------------------------------------------------------
# 11.2. CLASIFICAR LAS COMBINACIONES REPETIDAS
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("11.2. CLASIFICACIÓN DE LAS REPETICIONES\n")
cat("============================================================\n")


diagnostico_repeticiones <- claves_repetidas_universos %>%
  
  mutate(
    
    tipo_repeticion = case_when(
      
      universos_con_valor_disponible == 0 ~
        "Sin valores disponibles para comparar",
      
      universos_con_valor_disponible == 1 ~
        "Un solo universo con valor disponible",
      
      valores_distintos_disponibles == 1 ~
        "Valores disponibles idénticos",
      
      valores_distintos_disponibles > 1 ~
        "Valores disponibles diferentes",
      
      TRUE ~
        "Revisar"
    )
  )


cat("\nDistribución según coincidencia de valores:\n")

print(
  diagnostico_repeticiones %>%
    
    count(
      tipo_repeticion,
      name = "combinaciones"
    ) %>%
    
    arrange(
      desc(combinaciones)
    ),
  n = Inf
)


# ------------------------------------------------------------
# 11.3. RESUMEN GENERAL DE LAS REPETICIONES
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("11.3. RESUMEN GENERAL DE LAS REPETICIONES\n")
cat("============================================================\n")


resumen_repeticiones <- diagnostico_repeticiones %>%
  
  summarise(
    
    combinaciones_repetidas = n(),
    
    combinaciones_valores_identicos = sum(
      tipo_repeticion == "Valores disponibles idénticos"
    ),
    
    combinaciones_valores_diferentes = sum(
      tipo_repeticion == "Valores disponibles diferentes"
    ),
    
    combinaciones_un_solo_valor_disponible = sum(
      tipo_repeticion == "Un solo universo con valor disponible"
    ),
    
    combinaciones_sin_valores_disponibles = sum(
      tipo_repeticion == "Sin valores disponibles para comparar"
    ),
    
    universos_minimos = min(
      numero_universos
    ),
    
    universos_maximos = max(
      numero_universos
    )
  )


print(
  resumen_repeticiones,
  width = Inf
)


# ------------------------------------------------------------
# 11.4. DISTRIBUCIÓN SEGÚN EL NÚMERO DE UNIVERSOS
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("11.4. DISTRIBUCIÓN SEGÚN NÚMERO DE UNIVERSOS\n")
cat("============================================================\n")


distribucion_universos <- diagnostico_repeticiones %>%
  
  count(
    numero_universos,
    tipo_repeticion,
    name = "combinaciones"
  ) %>%
  
  arrange(
    numero_universos,
    tipo_repeticion
  )


print(
  distribucion_universos,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------
# 11.5. IDENTIFICAR COMBINACIONES CON VALORES DIFERENTES
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("11.5. REPETICIONES CON VALORES DISPONIBLES DIFERENTES\n")
cat("============================================================\n")


repeticiones_diferentes <- diagnostico_repeticiones %>%
  
  filter(
    tipo_repeticion == "Valores disponibles diferentes"
  ) %>%
  
  arrange(
    desc(numero_universos),
    provincia,
    anio_declaracion,
    anio_defuncion,
    edad_quinquenal
  )


cat("\nNúmero de combinaciones con valores diferentes:\n")

print(
  nrow(repeticiones_diferentes)
)


cat("\nPrimeras 30 combinaciones con valores diferentes:\n")

print(
  repeticiones_diferentes,
  n = 30,
  width = Inf
)


# ------------------------------------------------------------
# 11.6. RECUPERAR LOS VALORES ORIGINALES DE CASOS DIFERENTES
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("11.6. EXAMEN DE CASOS CON VALORES DIFERENTES\n")
cat("============================================================\n")


# Seleccionar hasta cinco combinaciones para inspección.

casos_para_revisar <- repeticiones_diferentes %>%
  
  slice_head(
    n = 5
  ) %>%
  
  select(
    sexo,
    provincia_id,
    provincia,
    anio_declaracion,
    anio_defuncion,
    edad_quinquenal
  )


cat("\nCombinaciones seleccionadas:\n")

print(
  casos_para_revisar,
  n = Inf,
  width = Inf
)


# Recuperar los registros originales sin agregarlos.
#
# Se conserva categoria_disponible para distinguir
# los valores numéricos de los no disponibles.

detalle_repeticiones <- datos_completos %>%
  
  semi_join(
    casos_para_revisar,
    
    by = c(
      "sexo",
      "provincia_id",
      "provincia",
      "anio_declaracion",
      "anio_defuncion",
      "edad_quinquenal"
    )
  ) %>%
  
  select(
    universo,
    sexo,
    provincia_id,
    provincia,
    anio_declaracion,
    anio_defuncion,
    edad_quinquenal,
    categoria_disponible,
    defunciones
  ) %>%
  
  arrange(
    provincia_id,
    sexo,
    anio_declaracion,
    anio_defuncion,
    edad_quinquenal,
    universo
  )


cat("\nValores originales de las combinaciones seleccionadas:\n")

print(
  detalle_repeticiones,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------
# 11.7. RECUPERAR TODAS LAS COMBINACIONES REPETIDAS
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("11.7. RECUPERACIÓN DE TODAS LAS REPETICIONES\n")
cat("============================================================\n")


# Se recuperan los registros originales correspondientes
# a todas las claves repetidas.
#
# No se suman ni se eliminan registros.
#
# Se conserva la variable de disponibilidad.

detalle_todas_repeticiones <- datos_completos %>%
  
  semi_join(
    claves_repetidas_universos,
    
    by = c(
      "sexo",
      "provincia_id",
      "provincia",
      "anio_declaracion",
      "anio_defuncion",
      "edad_quinquenal"
    )
  )


cat("\nDimensiones del detalle de repeticiones:\n")

print(
  dim(detalle_todas_repeticiones)
)


# ------------------------------------------------------------
# 11.8. EXAMINAR LOS PATRONES DE CEROS Y VALORES POSITIVOS
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("11.8. PATRONES DE CEROS Y VALORES POSITIVOS\n")
cat("============================================================\n")


patron_repeticiones <- detalle_todas_repeticiones %>%
  
  group_by(
    sexo,
    provincia_id,
    provincia,
    anio_declaracion,
    anio_defuncion,
    edad_quinquenal
  ) %>%
  
  summarise(
    
    # Número total de universos en los que aparece la clave.
    
    numero_universos = n_distinct(universo),
    
    # Número de universos con valores disponibles.
    
    universos_con_valor_disponible = n_distinct(
      universo[categoria_disponible]
    ),
    
    # Número de universos sin valores disponibles.
    
    universos_sin_valor_disponible = n_distinct(
      universo[!categoria_disponible]
    ),
    
    # Número de universos con conteos positivos.
    #
    # Se consideran exclusivamente valores disponibles.
    
    universos_con_defunciones = n_distinct(
      universo[
        categoria_disponible &
          defunciones > 0
      ]
    ),
    
    # Número de universos con conteos iguales a cero.
    
    universos_con_cero = n_distinct(
      universo[
        categoria_disponible &
          defunciones == 0
      ]
    ),
    
    # Suma diagnóstica de los conteos disponibles.
    #
    # No representa necesariamente defunciones únicas.
    
    total_defunciones_suma = sum(
      defunciones[categoria_disponible]
    ),
    
    # Mayor conteo observado entre valores disponibles.
    
    maximo_defunciones = if (
      any(categoria_disponible)
    ) {
      max(defunciones[categoria_disponible])
    } else {
      NA_real_
    },
    
    .groups = "drop"
  ) %>%
  
  mutate(
    
    patron = case_when(
      
      universos_con_defunciones == 0 ~
        "Ningún universo tiene valores positivos",
      
      universos_con_defunciones == 1 ~
        "Un solo universo tiene valores positivos",
      
      universos_con_defunciones > 1 ~
        "Varios universos tienen valores positivos",
      
      TRUE ~
        "Revisar"
    )
  )


cat("\nMuestra de los patrones identificados:\n")

print(
  patron_repeticiones,
  n = 30,
  width = Inf
)


# ------------------------------------------------------------
# 11.9. RESUMIR LOS PATRONES DE REPETICIÓN
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("11.9. RESUMEN DE LOS PATRONES DE REPETICIÓN\n")
cat("============================================================\n")


resumen_patrones <- patron_repeticiones %>%
  
  count(
    patron,
    name = "combinaciones"
  ) %>%
  
  mutate(
    
    porcentaje = round(
      100 * combinaciones / sum(combinaciones),
      2
    )
  ) %>%
  
  arrange(
    desc(combinaciones)
  )


print(
  resumen_patrones,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------
# 11.10. IDENTIFICAR VALORES POSITIVOS EN VARIOS UNIVERSOS
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("11.10. VALORES POSITIVOS EN VARIOS UNIVERSOS\n")
cat("============================================================\n")


positivos_multiples_universos <- patron_repeticiones %>%
  
  filter(
    universos_con_defunciones > 1
  ) %>%
  
  arrange(
    desc(universos_con_defunciones),
    provincia,
    anio_declaracion,
    anio_defuncion,
    edad_quinquenal
  )


cat("\nCombinaciones positivas en varios universos:\n")

print(
  nrow(positivos_multiples_universos)
)


cat("\nPrimeras 30 combinaciones:\n")

print(
  positivos_multiples_universos,
  n = 30,
  width = Inf
)


# ------------------------------------------------------------
# 11.11. RECUPERAR EL DETALLE DE LOS VALORES POSITIVOS
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("11.11. DETALLE DE POSITIVOS EN VARIOS UNIVERSOS\n")
cat("============================================================\n")


detalle_positivos_multiples <- datos_completos %>%
  
  semi_join(
    positivos_multiples_universos,
    
    by = c(
      "sexo",
      "provincia_id",
      "provincia",
      "anio_declaracion",
      "anio_defuncion",
      "edad_quinquenal"
    )
  ) %>%
  
  filter(
    categoria_disponible,
    defunciones > 0
  ) %>%
  
  select(
    universo,
    sexo,
    provincia_id,
    provincia,
    anio_declaracion,
    anio_defuncion,
    edad_quinquenal,
    categoria_disponible,
    defunciones
  ) %>%
  
  arrange(
    provincia_id,
    sexo,
    anio_declaracion,
    anio_defuncion,
    edad_quinquenal,
    universo
  )


cat("\nPrimeras 50 filas del detalle:\n")

print(
  detalle_positivos_multiples,
  n = 50,
  width = Inf
)


# ------------------------------------------------------------
# 11.12. RESUMEN DE REPETICIONES POR UNIVERSO
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("11.12. REPETICIONES POR UNIVERSO\n")
cat("============================================================\n")


resumen_positivos_universo <- detalle_todas_repeticiones %>%
  
  group_by(
    universo
  ) %>%
  
  summarise(
    
    filas_repetidas = n(),
    
    # Filas sin valores disponibles.
    
    filas_no_disponibles = sum(
      !categoria_disponible
    ),
    
    # Filas disponibles con valor cero.
    
    filas_con_cero = sum(
      categoria_disponible &
        defunciones == 0
    ),
    
    # Filas disponibles con valores positivos.
    
    filas_con_defunciones = sum(
      categoria_disponible &
        defunciones > 0
    ),
    
    # Suma exclusivamente los valores disponibles.
    
    total_defunciones_repetidas = sum(
      defunciones[categoria_disponible]
    ),
    
    .groups = "drop"
  ) %>%
  
  arrange(
    universo
  )


print(
  resumen_positivos_universo,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------
# 11.13. RESUMEN FINAL DEL DIAGNÓSTICO
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("11.13. RESUMEN FINAL DEL DIAGNÓSTICO\n")
cat("============================================================\n")


resumen_final_repeticiones <- tibble(
  
  combinaciones_repetidas = nrow(
    claves_repetidas_universos
  ),
  
  combinaciones_valores_identicos = sum(
    diagnostico_repeticiones$tipo_repeticion ==
      "Valores disponibles idénticos"
  ),
  
  combinaciones_valores_diferentes = sum(
    diagnostico_repeticiones$tipo_repeticion ==
      "Valores disponibles diferentes"
  ),
  
  combinaciones_un_solo_valor_disponible = sum(
    diagnostico_repeticiones$tipo_repeticion ==
      "Un solo universo con valor disponible"
  ),
  
  combinaciones_sin_valores_disponibles = sum(
    diagnostico_repeticiones$tipo_repeticion ==
      "Sin valores disponibles para comparar"
  ),
  
  positivas_en_un_solo_universo = sum(
    patron_repeticiones$universos_con_defunciones == 1
  ),
  
  positivas_en_varios_universos = sum(
    patron_repeticiones$universos_con_defunciones > 1
  ),
  
  sin_defunciones_positivas = sum(
    patron_repeticiones$universos_con_defunciones == 0
  )
)


print(
  resumen_final_repeticiones,
  width = Inf
)


# ------------------------------------------------------------
# 11.14. VALIDACIONES DE INTEGRIDAD
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("11.14. VALIDACIONES DE INTEGRIDAD\n")
cat("============================================================\n")


comprobaciones_seccion_11 <- c(
  
  # Todas las claves identificadas deben aparecer
  # en más de un universo.
  
  claves_con_varios_universos =
    all(
      claves_repetidas_universos$numero_universos > 1
    ),
  
  # Los valores disponibles no deben contener faltantes.
  
  valores_disponibles_sin_faltantes =
    !anyNA(
      detalle_todas_repeticiones$defunciones[
        detalle_todas_repeticiones$categoria_disponible
      ]
    ),
  
  # La clasificación debe conservar todas las claves.
  
  clasificacion_completa =
    nrow(diagnostico_repeticiones) ==
    nrow(claves_repetidas_universos),
  
  # Las categorías de clasificación deben reconstruir
  # el total de combinaciones repetidas.
  
  clasificacion_reconstruye_total =
    sum(
      resumen_final_repeticiones$combinaciones_valores_identicos,
      resumen_final_repeticiones$combinaciones_valores_diferentes,
      resumen_final_repeticiones$combinaciones_un_solo_valor_disponible,
      resumen_final_repeticiones$combinaciones_sin_valores_disponibles
    ) ==
    resumen_final_repeticiones$combinaciones_repetidas,
  
  # Los patrones deben reconstruir todas las combinaciones.
  
  patrones_reconstruyen_total =
    sum(resumen_patrones$combinaciones) ==
    nrow(patron_repeticiones),
  
  # Las tres categorías de patrones deben reconstruir
  # el total de combinaciones repetidas.
  
  categorias_patrones_reconstruyen_total =
    sum(
      resumen_final_repeticiones$positivas_en_un_solo_universo,
      resumen_final_repeticiones$positivas_en_varios_universos,
      resumen_final_repeticiones$sin_defunciones_positivas
    ) ==
    nrow(patron_repeticiones),
  
  # El detalle de valores positivos debe contener
  # exclusivamente conteos disponibles mayores que cero.
  
  detalle_solo_positivos =
    all(
      detalle_positivos_multiples$categoria_disponible &
        detalle_positivos_multiples$defunciones > 0
    ),
  
  # Las combinaciones positivas en varios universos
  # deben tener más de un universo positivo.
  
  multiples_positivos_correctos =
    all(
      positivos_multiples_universos$universos_con_defunciones > 1
    ),
  
  # Los universos con valores disponibles y no disponibles
  # deben reconstruir el número total de universos.
  
  disponibilidad_reconstruye_universos =
    all(
      diagnostico_repeticiones$universos_con_valor_disponible +
        diagnostico_repeticiones$universos_sin_valor_disponible ==
        diagnostico_repeticiones$numero_universos
    ),
  
  # Las filas no disponibles no deben contarse como ceros
  # ni como valores positivos.
  
  no_disponibles_no_clasificados_como_cero_o_positivos =
    all(
      detalle_todas_repeticiones$universo[
        !detalle_todas_repeticiones$categoria_disponible
      ] %in%
        detalle_todas_repeticiones$universo
    )
  
)


cat("\nResultados de las comprobaciones:\n")


# ------------------------------------------------------------
# 11.14. VALIDACIONES DE INTEGRIDAD
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("11.14. VALIDACIONES DE INTEGRIDAD\n")
cat("============================================================\n")


comprobaciones_seccion_11 <- c(
  
  # Todas las claves identificadas deben aparecer
  # en más de un universo.
  
  claves_con_varios_universos =
    all(
      claves_repetidas_universos$numero_universos > 1
    ),
  
  # Los valores disponibles no deben contener faltantes.
  
  valores_disponibles_sin_faltantes =
    !anyNA(
      detalle_todas_repeticiones$defunciones[
        detalle_todas_repeticiones$categoria_disponible
      ]
    ),
  
  # La clasificación debe conservar todas las claves.
  
  clasificacion_completa =
    nrow(diagnostico_repeticiones) ==
    nrow(claves_repetidas_universos),
  
  # Las categorías de clasificación deben reconstruir
  # el total de combinaciones repetidas.
  
  clasificacion_reconstruye_total =
    sum(
      resumen_final_repeticiones$combinaciones_valores_identicos,
      resumen_final_repeticiones$combinaciones_valores_diferentes,
      resumen_final_repeticiones$combinaciones_un_solo_valor_disponible,
      resumen_final_repeticiones$combinaciones_sin_valores_disponibles
    ) ==
    resumen_final_repeticiones$combinaciones_repetidas,
  
  # Los patrones deben reconstruir todas las combinaciones.
  
  patrones_reconstruyen_total =
    sum(resumen_patrones$combinaciones) ==
    nrow(patron_repeticiones),
  
  # Las tres categorías de patrones deben reconstruir
  # el total de combinaciones repetidas.
  
  categorias_patrones_reconstruyen_total =
    sum(
      resumen_final_repeticiones$positivas_en_un_solo_universo,
      resumen_final_repeticiones$positivas_en_varios_universos,
      resumen_final_repeticiones$sin_defunciones_positivas
    ) ==
    nrow(patron_repeticiones),
  
  # El detalle de valores positivos debe contener
  # exclusivamente conteos disponibles mayores que cero.
  
  detalle_solo_positivos =
    all(
      detalle_positivos_multiples$categoria_disponible &
        !is.na(detalle_positivos_multiples$defunciones) &
        detalle_positivos_multiples$defunciones > 0
    ),
  
  # Las combinaciones positivas en varios universos
  # deben tener más de un universo positivo.
  
  multiples_positivos_correctos =
    all(
      positivos_multiples_universos$universos_con_defunciones > 1
    ),
  
  # Los universos con valores disponibles y no disponibles
  # deben reconstruir el número total de universos.
  
  disponibilidad_reconstruye_universos =
    all(
      diagnostico_repeticiones$universos_con_valor_disponible +
        diagnostico_repeticiones$universos_sin_valor_disponible ==
        diagnostico_repeticiones$numero_universos
    ),
  
  # Las filas disponibles con cero o valores positivos,
  # junto con las no disponibles, deben reconstruir
  # todas las filas repetidas de cada universo.
  
  clasificacion_filas_por_disponibilidad_correcta =
    all(
      resumen_positivos_universo$filas_con_cero +
        resumen_positivos_universo$filas_con_defunciones +
        resumen_positivos_universo$filas_no_disponibles ==
        resumen_positivos_universo$filas_repetidas
    )
  
)


cat("\nResultados de las comprobaciones:\n")

print(
  tibble(
    comprobacion = names(comprobaciones_seccion_11),
    resultado = unname(comprobaciones_seccion_11)
  ),
  n = Inf
)


# Detener la ejecución si alguna comprobación falla.

stopifnot(
  all(comprobaciones_seccion_11)
)


cat("\n============================================================\n")
cat("SECCIÓN 11 COMPLETADA.\n")
cat("TODAS LAS VALIDACIONES DEL DIAGNÓSTICO FUERON SUPERADAS.\n")
cat("============================================================\n")


# ============================================================
# 11.15. SUMMARY TABLE AND GRAPHICAL PRESENTATION
# ============================================================

# OBJECTIVE:
#
# To produce a descriptive summary of the repeated
# combinations identified across annual REDATAM universes.
#
# Two dimensions are presented:
#
# 1. Comparison of available values across universes.
# 2. Distribution of repeated combinations according to
#    the number of universes containing positive counts.
#
# The unit of analysis is the repeated combination of:
# sex, province, declaration year, year of death,
# and age group.
#
# These results describe similarities and differences
# across independent annual universes. They do not
# represent cumulative database updates or unique deaths.


# ------------------------------------------------------------
# 11.15.1. BUILD THE SUMMARY TABLE
# ------------------------------------------------------------

total_combinaciones_repetidas <-
  resumen_final_repeticiones$combinaciones_repetidas[[1]]


tabla_resumen_seccion_11 <- tibble::tibble(
  
  diagnostic = c(
    rep(
      "Comparison of available values",
      4
    ),
    rep(
      "Distribution of positive counts",
      3
    )
  ),
  
  category = c(
    
    "Identical available values",
    "Different available values",
    "Only one universe with an available value",
    "No available values",
    
    "Positive counts in one universe only",
    "Positive counts in multiple universes",
    "No positive counts"
    
  ),
  
  n = c(
    
    resumen_final_repeticiones$
      combinaciones_valores_identicos[[1]],
    
    resumen_final_repeticiones$
      combinaciones_valores_diferentes[[1]],
    
    resumen_final_repeticiones$
      combinaciones_un_solo_valor_disponible[[1]],
    
    resumen_final_repeticiones$
      combinaciones_sin_valores_disponibles[[1]],
    
    resumen_final_repeticiones$
      positivas_en_un_solo_universo[[1]],
    
    resumen_final_repeticiones$
      positivas_en_varios_universos[[1]],
    
    resumen_final_repeticiones$
      sin_defunciones_positivas[[1]]
    
  )
  
) %>%
  
  group_by(
    diagnostic
  ) %>%
  
  mutate(
    
    total_combinations = sum(n),
    
    percentage = 100 * n / total_combinations
    
  ) %>%
  
  ungroup() %>%
  
  mutate(
    
    percentage = round(
      percentage,
      1
    )
    
  )


# ------------------------------------------------------------
# 11.15.2. DISPLAY THE SUMMARY TABLE
# ------------------------------------------------------------

cat("\n============================================================\n")

cat("11.15. SUMMARY OF REPEATED COMBINATIONS\n")

cat("============================================================\n")


print(
  tabla_resumen_seccion_11,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------
# 11.15.3. VALIDATE THE SUMMARY TABLE
# ------------------------------------------------------------

validaciones_resumen_seccion_11 <- c(
  
  correct_number_of_repeated_combinations =
    all(
      tabla_resumen_seccion_11$total_combinations ==
        total_combinaciones_repetidas
    ),
  
  comparison_categories_reconstruct_total =
    sum(
      tabla_resumen_seccion_11$n[
        tabla_resumen_seccion_11$diagnostic ==
          "Comparison of available values"
      ]
    ) ==
    total_combinaciones_repetidas,
  
  positive_count_categories_reconstruct_total =
    sum(
      tabla_resumen_seccion_11$n[
        tabla_resumen_seccion_11$diagnostic ==
          "Distribution of positive counts"
      ]
    ) ==
    total_combinaciones_repetidas,
  
  percentages_reconstruct_each_diagnostic =
    all(
      abs(
        tabla_resumen_seccion_11 %>%
          group_by(diagnostic) %>%
          summarise(
            total_percentage = sum(percentage),
            .groups = "drop"
          ) %>%
          pull(total_percentage) -
          100
      ) <= 0.2
    )
  
)


cat("\nSummary table validation:\n")

print(
  tibble::tibble(
    validation = names(validaciones_resumen_seccion_11),
    result = unname(validaciones_resumen_seccion_11)
  ),
  n = Inf
)


stopifnot(
  all(validaciones_resumen_seccion_11)
)




# ------------------------------------------------------------
# 11.15.4. CREATE THE IMPROVED SUMMARY GRAPH
# ------------------------------------------------------------

# Define category order and labels within the plotting dataset.

datos_grafico_seccion_11 <- tabla_resumen_seccion_11 %>%
  
  mutate(
    
    category = factor(
      
      category,
      
      levels = c(
        
        "Identical available values",
        "Different available values",
        "Only one universe with an available value",
        "No available values",
        
        "Positive counts in one universe only",
        "Positive counts in multiple universes",
        "No positive counts"
        
      )
      
    ),
    
    diagnostic = factor(
      
      diagnostic,
      
      levels = c(
        
        "Comparison of available values",
        "Distribution of positive counts"
        
      )
      
    ),
    
    etiqueta = paste0(
      
      format(
        n,
        big.mark = ",",
        scientific = FALSE
      ),
      
      " (",
      
      sprintf("%.1f", percentage),
      
      "%)"
      
    )
    
  )


# Create the graph.

grafico_resumen_seccion_11 <- ggplot2::ggplot(
  
  datos_grafico_seccion_11,
  
  ggplot2::aes(
    
    x = percentage,
    
    y = category
    
  )
  
) +
  
  ggplot2::geom_col(
    
    width = 0.65
    
  ) +
  
  ggplot2::geom_text(
    
    ggplot2::aes(
      label = etiqueta
    ),
    
    hjust = -0.10,
    
    size = 3.5
    
  ) +
  
  ggplot2::facet_wrap(
    
    ~ diagnostic,
    
    scales = "free_y",
    
    ncol = 1
    
  ) +
  
  ggplot2::scale_x_continuous(
    
    breaks = seq(
      0,
      100,
      by = 20
    ),
    
    labels = function(x) {
      paste0(x, "%")
    },
    
    expand = ggplot2::expansion(
      mult = c(0, 0),
      add = c(0, 8)
    )
    
  ) +
  
  ggplot2::scale_y_discrete(
    
    drop = FALSE
    
  ) +
  
  ggplot2::labs(
    
    title = "Repeated combinations across annual REDATAM universes",
    
    subtitle = paste0(
      format(
        total_combinaciones_repetidas,
        big.mark = ","
      ),
      " repeated combinations identified"
    ),
    
    x = "Percentage of repeated combinations",
    
    y = NULL,
    
    caption = paste(
      "Note: Annual universes are independent.",
      "Percentages are calculated within each diagnostic dimension.",
      "Repeated combinations do not represent unique deaths."
    )
    
  ) +
  
  ggplot2::coord_cartesian(
    
    clip = "off"
    
  ) +
  
  ggplot2::theme_minimal(
    
    base_size = 12
    
  ) +
  
  ggplot2::theme(
    
    plot.title = ggplot2::element_text(
      face = "bold",
      size = 14
    ),
    
    plot.subtitle = ggplot2::element_text(
      size = 11
    ),
    
    strip.text = ggplot2::element_text(
      face = "bold",
      size = 11
    ),
    
    panel.grid.major.y = ggplot2::element_blank(),
    
    panel.grid.minor = ggplot2::element_blank(),
    
    panel.spacing = grid::unit(
      1.2,
      "lines"
    ),
    
    plot.caption = ggplot2::element_text(
      size = 9,
      hjust = 0
    ),
    
    plot.margin = ggplot2::margin(
      10,
      35,
      10,
      10
    )
    
  )


# ------------------------------------------------------------
# 11.15.5. DISPLAY THE IMPROVED GRAPH
# ------------------------------------------------------------

print(
  grafico_resumen_seccion_11
)


# ------------------------------------------------------------
# 11.15.6. CONFIRM GRAPH GENERATION
# ------------------------------------------------------------

cat("\n============================================================\n")

cat("IMPROVED SUMMARY GRAPH GENERATED SUCCESSFULLY.\n")

cat("============================================================\n")



# ============================================================
#
# 12. COMPARACIÓN DE LOS UNIVERSOS REDATAM CONSECUTIVOS
#
# ============================================================
#
# OBJETIVO:
#
# Comparar los archivos REDATAM de años consecutivos
# para examinar los cambios en las combinaciones agregadas.
#
# Se busca identificar:
#
# 1. Combinaciones que aparecen en el universo siguiente.
# 2. Combinaciones que desaparecen.
# 3. Combinaciones cuyos valores aumentan.
# 4. Combinaciones cuyos valores disminuyen.
# 5. Combinaciones cuyos valores permanecen iguales.
# 6. Cambios en los totales por universo.
# 7. Cambios según el año de defunción.
#
# IMPORTANTE:
#
# - La unidad de análisis es una combinación agregada.
# - No se identifican personas fallecidas individuales.
# - Los universos son conjuntos anuales independientes.
# - No se suman los universos para estimar defunciones únicas.
# - Los valores no disponibles no se interpretan como ceros.
# - La comparación es exclusivamente diagnóstica.
#
# ============================================================


# ------------------------------------------------------------
# 12.1. PREPARAR LA BASE PARA LA COMPARACIÓN
# ------------------------------------------------------------

# universo identifica el año del archivo REDATAM.
#
# Se conserva categoria_disponible para distinguir
# los valores observados de los no disponibles.

datos_comparacion_anual <- datos_completos %>%
  
  transmute(
    
    anio_trabajo = universo,
    
    sexo,
    provincia_id,
    anio_declaracion,
    anio_defuncion,
    edad_quinquenal,
    
    categoria_disponible,
    defunciones
    
  )


# Resumen general de la base.

cat("\nDimensiones de la base de comparación:\n")

print(dim(datos_comparacion_anual))


cat("\nAños de trabajo disponibles:\n")

universos_presentes <- sort(
  unique(datos_comparacion_anual$anio_trabajo)
)

print(universos_presentes)


resumen_base_comparacion <- datos_comparacion_anual %>%
  
  summarise(
    
    filas = n(),
    
    universos = n_distinct(anio_trabajo),
    
    valores_disponibles = sum(
      categoria_disponible
    ),
    
    valores_no_disponibles = sum(
      !categoria_disponible
    ),
    
    total_defunciones = sum(
      defunciones,
      na.rm = TRUE
    )
    
  )


cat("\nResumen general de la base de comparación:\n")

print(resumen_base_comparacion)


# Comprobar coherencia entre disponibilidad y valores.

if (
  anyNA(datos_comparacion_anual$categoria_disponible) ||
  any(
    datos_comparacion_anual$categoria_disponible &
    is.na(datos_comparacion_anual$defunciones)
  ) ||
  any(
    !datos_comparacion_anual$categoria_disponible &
    !is.na(datos_comparacion_anual$defunciones)
  )
) {
  
  stop(
    "Existe una inconsistencia entre categoria_disponible ",
    "y los valores de defunciones."
  )
  
}


# ------------------------------------------------------------
# 12.2. VERIFICAR UNICIDAD DE LAS CLAVES
#       DENTRO DE CADA UNIVERSO
# ------------------------------------------------------------

# Las claves identifican cada combinación agregada
# dentro de un universo específico.
#
# No se incluye el nombre de provincia porque
# provincia_id es su identificador territorial.

claves_comparacion <- c(
  
  "sexo",
  "provincia_id",
  "anio_declaracion",
  "anio_defuncion",
  "edad_quinquenal"
  
)


# Identificar posibles duplicados.

duplicados_comparacion <- datos_comparacion_anual %>%
  
  count(
    anio_trabajo,
    across(all_of(claves_comparacion)),
    name = "frecuencia"
  ) %>%
  
  filter(
    frecuencia > 1
  )


cat("\nNúmero de claves duplicadas dentro de los universos:\n")

print(nrow(duplicados_comparacion))


# Detener el proceso si existen duplicados.

if (nrow(duplicados_comparacion) > 0) {
  
  print(
    duplicados_comparacion,
    n = Inf
  )
  
  stop(
    "Existen claves duplicadas dentro de un universo. ",
    "Debe revisarse la base antes de continuar."
  )
  
}


# ------------------------------------------------------------
# 12.3. CREAR FUNCIÓN DE COMPARACIÓN ENTRE DOS UNIVERSOS
#       CONSECUTIVOS
# ------------------------------------------------------------

comparar_dos_universos <- function(anio_t, datos) {
  
  anio_t1 <- anio_t + 1
  
  
  # Extraer las combinaciones del primer universo.
  
  datos_t <- datos %>%
    
    filter(
      anio_trabajo == anio_t
    ) %>%
    
    transmute(
      
      across(all_of(claves_comparacion)),
      
      defunciones_t = defunciones,
      
      disponible_t = categoria_disponible,
      
      presente_t = TRUE
      
    )
  
  
  # Extraer las combinaciones del universo siguiente.
  
  datos_t1 <- datos %>%
    
    filter(
      anio_trabajo == anio_t1
    ) %>%
    
    transmute(
      
      across(all_of(claves_comparacion)),
      
      defunciones_t1 = defunciones,
      
      disponible_t1 = categoria_disponible,
      
      presente_t1 = TRUE
      
    )
  
  
  # Unir las combinaciones de ambos universos.
  
  comparacion <- full_join(
    
    datos_t,
    datos_t1,
    
    by = claves_comparacion
    
  ) %>%
    
    mutate(
      
      # Identificar presencia en cada universo.
      
      presente_t = coalesce(
        presente_t,
        FALSE
      ),
      
      presente_t1 = coalesce(
        presente_t1,
        FALSE
      ),
      
      
      # Identificar disponibilidad de los valores.
      #
      # Una combinación ausente no tiene un valor disponible.
      # Una combinación presente puede tener un valor
      # no disponible, que debe conservarse como tal.
      
      disponible_t = coalesce(
        disponible_t,
        FALSE
      ),
      
      disponible_t1 = coalesce(
        disponible_t1,
        FALSE
      ),
      
      
      # Calcular el retraso de declaración.
      
      retraso_declaracion =
        anio_declaracion - anio_defuncion,
      
      
      # Calcular diferencias únicamente cuando ambos
      # universos contienen la combinación y los dos
      # valores están disponibles.
      
      diferencia = if_else(
        
        presente_t &
          presente_t1 &
          disponible_t &
          disponible_t1,
        
        defunciones_t1 - defunciones_t,
        
        NA_real_
        
      ),
      
      cambio_absoluto = abs(diferencia),
      
      
      # Clasificar los patrones de cambio.
      #
      # La aparición y desaparición se refieren a la
      # presencia de la combinación, no necesariamente
      # a la disponibilidad de su valor.
      
      patron_cambio = case_when(
        
        !presente_t & presente_t1 ~
          "Aparece en t+1",
        
        presente_t & !presente_t1 ~
          "Desaparece en t+1",
        
        presente_t &
          presente_t1 &
          (!disponible_t | !disponible_t1) ~
          "No comparable: valor no disponible",
        
        diferencia > 0 ~
          "Aumenta",
        
        diferencia < 0 ~
          "Disminuye",
        
        diferencia == 0 ~
          "Sin cambio",
        
        TRUE ~ NA_character_
        
      ),
      
      
      # Clasificar según el retraso de declaración.
      
      categoria_retraso = case_when(
        
        retraso_declaracion < 0 ~
          "Anomalía temporal",
        
        retraso_declaracion == 0 ~
          "Declaración mismo año",
        
        retraso_declaracion > 0 ~
          "Declaración tardía",
        
        TRUE ~ NA_character_
        
      ),
      
      
      # Identificar los universos comparados.
      
      anio_trabajo_t = anio_t,
      
      anio_trabajo_t1 = anio_t1
      
    ) %>%
    
    select(
      
      anio_trabajo_t,
      anio_trabajo_t1,
      
      all_of(claves_comparacion),
      
      retraso_declaracion,
      categoria_retraso,
      
      defunciones_t,
      defunciones_t1,
      
      disponible_t,
      disponible_t1,
      
      presente_t,
      presente_t1,
      
      diferencia,
      cambio_absoluto,
      
      patron_cambio
      
    )
  
  
  comparacion
  
}


# ------------------------------------------------------------
# 12.4. COMPARAR TODOS LOS AÑOS CONSECUTIVOS
# ------------------------------------------------------------

# Los universos disponibles deben cubrir 2000–2025.
#
# Se comparan los 25 pares:
# 2000–2001, 2001–2002, ..., 2024–2025.

universos_esperados <- 2000:2025


# Comprobar que están presentes todos los universos.

if (!identical(
  universos_presentes,
  universos_esperados
)) {
  
  stop(
    "Los universos disponibles no corresponden ",
    "exactamente al intervalo 2000–2025."
  )
  
}


# Construir las comparaciones consecutivas.

anios_comparacion <- head(
  universos_esperados,
  -1
)


comparaciones_consecutivas <- purrr::map_dfr(
  
  anios_comparacion,
  
  comparar_dos_universos,
  
  datos = datos_comparacion_anual
  
)


# Examinar dimensiones.

cat("\nDimensiones de las comparaciones consecutivas:\n")

print(dim(comparaciones_consecutivas))


# Comprobar pares de universos comparados.

pares_universos_comparados <- comparaciones_consecutivas %>%
  
  distinct(
    anio_trabajo_t,
    anio_trabajo_t1
  ) %>%
  
  arrange(
    anio_trabajo_t
  )


cat("\nPares de universos comparados:\n")

print(
  pares_universos_comparados,
  n = Inf
)


# Examinar las primeras observaciones.

cat("\nPrimeras observaciones:\n")

print(
  head(comparaciones_consecutivas)
)


# ------------------------------------------------------------
# 12.5. IDENTIFICAR LOS PATRONES DE CAMBIO
#       POR PAR DE UNIVERSOS
# ------------------------------------------------------------

resumen_patrones_por_anio <- comparaciones_consecutivas %>%
  
  count(
    anio_trabajo_t,
    anio_trabajo_t1,
    patron_cambio,
    name = "combinaciones"
  ) %>%
  
  group_by(
    anio_trabajo_t,
    anio_trabajo_t1
  ) %>%
  
  mutate(
    
    total_combinaciones = sum(combinaciones),
    
    porcentaje = 100 *
      combinaciones / total_combinaciones
    
  ) %>%
  
  ungroup() %>%
  
  arrange(
    anio_trabajo_t,
    patron_cambio
  )


cat("\nPatrones de cambio por par de universos:\n")

print(
  resumen_patrones_por_anio,
  n = Inf
)


# ------------------------------------------------------------
# 12.6. EXAMINAR LOS PATRONES DE CAMBIO SEGÚN
#       EL RETRASO DE DECLARACIÓN
# ------------------------------------------------------------

resumen_retraso_por_anio <- comparaciones_consecutivas %>%
  
  count(
    anio_trabajo_t,
    anio_trabajo_t1,
    categoria_retraso,
    patron_cambio,
    name = "combinaciones"
  ) %>%
  
  group_by(
    anio_trabajo_t,
    anio_trabajo_t1,
    categoria_retraso
  ) %>%
  
  mutate(
    
    total_categoria = sum(combinaciones),
    
    porcentaje = 100 *
      combinaciones / total_categoria
    
  ) %>%
  
  ungroup() %>%
  
  arrange(
    anio_trabajo_t,
    categoria_retraso,
    desc(combinaciones)
  )


cat("\nPatrones de cambio según retraso de declaración:\n")

print(
  resumen_retraso_por_anio,
  n = Inf
)


# ------------------------------------------------------------
# 12.7. COMPARAR LOS TOTALES ENTRE UNIVERSOS CONSECUTIVOS
# ------------------------------------------------------------

# Para calcular los totales:
#
# - Las combinaciones ausentes en un universo aportan cero.
# - Los valores no disponibles permanecen como NA.
# - sum(na.rm = TRUE) excluye únicamente los valores
#   no disponibles.
#
# Así se recuperan los totales de valores disponibles
# de cada universo sin convertir los NA en ceros.

resumen_totales_consecutivos <- comparaciones_consecutivas %>%
  
  group_by(
    anio_trabajo_t,
    anio_trabajo_t1
  ) %>%
  
  summarise(
    
    # Total del universo inicial.
    
    total_defunciones_t = sum(
      if_else(
        presente_t,
        defunciones_t,
        0
      ),
      na.rm = TRUE
    ),
    
    
    # Total del universo siguiente.
    
    total_defunciones_t1 = sum(
      if_else(
        presente_t1,
        defunciones_t1,
        0
      ),
      na.rm = TRUE
    ),
    
    
    # Diferencia entre los totales.
    
    diferencia_total =
      total_defunciones_t1 -
      total_defunciones_t,
    
    
    # Diferencia porcentual respecto al universo inicial.
    
    diferencia_porcentual = if_else(
      
      total_defunciones_t > 0,
      
      100 * diferencia_total /
        total_defunciones_t,
      
      NA_real_
      
    ),
    
    
    # Número de combinaciones presentes.
    
    combinaciones_t = sum(presente_t),
    
    combinaciones_t1 = sum(presente_t1),
    
    
    # Combinaciones compartidas.
    
    combinaciones_compartidas = sum(
      presente_t & presente_t1
    ),
    
    
    # Combinaciones exclusivas de cada universo.
    
    combinaciones_solo_t = sum(
      presente_t & !presente_t1
    ),
    
    combinaciones_solo_t1 = sum(
      !presente_t & presente_t1
    ),
    
    
    # Combinaciones compartidas con valores no disponibles
    # en al menos uno de los dos universos.
    
    combinaciones_no_comparables = sum(
      presente_t &
        presente_t1 &
        (!disponible_t | !disponible_t1)
    ),
    
    
    .groups = "drop"
    
  ) %>%
  
  arrange(
    anio_trabajo_t
  )


cat("\nComparación de totales entre universos consecutivos:\n")

print(
  resumen_totales_consecutivos,
  n = Inf
)


# ------------------------------------------------------------
# 12.8. VALIDAR LOS TOTALES RECUPERADOS
#       EN LAS COMPARACIONES
# ------------------------------------------------------------

# Recuperar los totales originales de cada universo.

totales_originales_universo <- datos_comparacion_anual %>%
  
  group_by(
    anio_trabajo
  ) %>%
  
  summarise(
    
    total_defunciones_original = sum(
      defunciones,
      na.rm = TRUE
    ),
    
    .groups = "drop"
    
  )


# Recuperar los totales de cada universo a partir
# de las comparaciones consecutivas.
#
# Cada universo intermedio aparece en dos pares.
# Se comprueba que ambas recuperaciones coincidan.

totales_comparaciones <- bind_rows(
  
  resumen_totales_consecutivos %>%
    
    transmute(
      
      anio_trabajo = anio_trabajo_t,
      
      total_defunciones_comparacion =
        total_defunciones_t
      
    ),
  
  
  resumen_totales_consecutivos %>%
    
    transmute(
      
      anio_trabajo = anio_trabajo_t1,
      
      total_defunciones_comparacion =
        total_defunciones_t1
      
    )
  
) %>%
  
  group_by(
    anio_trabajo
  ) %>%
  
  summarise(
    
    total_defunciones_comparacion =
      first(total_defunciones_comparacion),
    
    recuperaciones_distintas = n_distinct(
      total_defunciones_comparacion
    ),
    
    .groups = "drop"
    
  )


# Comparar los totales originales y recuperados.

validacion_totales_universo <- totales_originales_universo %>%
  
  left_join(
    totales_comparaciones,
    by = "anio_trabajo"
  ) %>%
  
  mutate(
    
    diferencia_validacion =
      total_defunciones_comparacion -
      total_defunciones_original,
    
    coincide =
      !is.na(diferencia_validacion) &
      diferencia_validacion == 0 &
      recuperaciones_distintas == 1
    
  ) %>%
  
  arrange(
    anio_trabajo
  )


cat("\nValidación de totales por universo:\n")

print(
  validacion_totales_universo,
  n = Inf
)


# ------------------------------------------------------------
# 12.9. COMPARAR LOS CAMBIOS SEGÚN EL AÑO DE DEFUNCIÓN
# ------------------------------------------------------------

# Este análisis permite observar si los cambios entre
# universos consecutivos se concentran en determinados
# años de ocurrencia de las defunciones.
#
# Se agregan las combinaciones de sexo, provincia y edad
# para obtener un total por año de defunción y universo.
#
# Los valores no disponibles se contabilizan por separado.
# No se interpretan los resultados como conteos de personas
# fallecidas únicas entre universos.


# 12.9.1. Calcular totales por universo y año de defunción.

totales_por_anio_defuncion <- datos_comparacion_anual %>%
  
  group_by(
    anio_trabajo,
    anio_defuncion
  ) %>%
  
  summarise(
    
    combinaciones = n(),
    
    valores_no_disponibles = sum(
      is.na(defunciones)
    ),
    
    defunciones = if (
      all(is.na(defunciones))
    ) {
      NA_real_
    } else {
      sum(defunciones, na.rm = TRUE)
    },
    
    .groups = "drop"
    
  )


# 12.9.2. Crear función de comparación por año de defunción.

comparar_anios_defuncion <- function(anio_t, datos) {
  
  anio_t1 <- anio_t + 1
  
  
  # Primer universo.
  
  datos_t <- datos %>%
    
    filter(
      anio_trabajo == anio_t
    ) %>%
    
    transmute(
      
      anio_defuncion,
      
      defunciones_t = defunciones,
      
      combinaciones_t = combinaciones,
      
      valores_no_disponibles_t =
        valores_no_disponibles,
      
      presente_t = TRUE
      
    )
  
  
  # Universo siguiente.
  
  datos_t1 <- datos %>%
    
    filter(
      anio_trabajo == anio_t1
    ) %>%
    
    transmute(
      
      anio_defuncion,
      
      defunciones_t1 = defunciones,
      
      combinaciones_t1 = combinaciones,
      
      valores_no_disponibles_t1 =
        valores_no_disponibles,
      
      presente_t1 = TRUE
      
    )
  
  
  # Comparar ambos universos.
  
  full_join(
    
    datos_t,
    datos_t1,
    
    by = "anio_defuncion"
    
  ) %>%
    
    mutate(
      
      presente_t = coalesce(
        presente_t,
        FALSE
      ),
      
      presente_t1 = coalesce(
        presente_t1,
        FALSE
      ),
      
      
      # Las ausencias de un año de defunción en un universo
      # se representan mediante cero.
      #
      # Si el año está presente pero todos sus valores
      # son NA, se conserva el NA.
      
      defunciones_t = if_else(
        presente_t,
        defunciones_t,
        0
      ),
      
      defunciones_t1 = if_else(
        presente_t1,
        defunciones_t1,
        0
      ),
      
      
      # Diferencia únicamente si ambos totales son conocidos.
      
      diferencia = if_else(
        
        !is.na(defunciones_t) &
          !is.na(defunciones_t1),
        
        defunciones_t1 - defunciones_t,
        
        NA_real_
        
      ),
      
      
      diferencia_porcentual = if_else(
        
        !is.na(diferencia) &
          defunciones_t > 0,
        
        100 * diferencia / defunciones_t,
        
        NA_real_
        
      ),
      
      
      # Identificar los universos comparados.
      
      anio_trabajo_t = anio_t,
      
      anio_trabajo_t1 = anio_t1
      
    ) %>%
    
    select(
      
      anio_trabajo_t,
      anio_trabajo_t1,
      anio_defuncion,
      
      presente_t,
      presente_t1,
      
      combinaciones_t,
      combinaciones_t1,
      
      valores_no_disponibles_t,
      valores_no_disponibles_t1,
      
      defunciones_t,
      defunciones_t1,
      
      diferencia,
      diferencia_porcentual
      
    )
  
}


# 12.9.3. Construir todas las comparaciones.

comparaciones_por_anio_defuncion <- purrr::map_dfr(
  
  anios_comparacion,
  
  comparar_anios_defuncion,
  
  datos = totales_por_anio_defuncion
  
) %>%
  
  arrange(
    anio_trabajo_t,
    anio_defuncion
  )


cat("\nComparaciones por año de defunción:\n")

print(
  comparaciones_por_anio_defuncion,
  n = 100
)


# ------------------------------------------------------------
# 12.10. VALIDACIONES GENERALES DE LA SECCIÓN 12
# ------------------------------------------------------------

# Comprobar la estructura y los resultados de las comparaciones.

validaciones_seccion_12 <- tibble(
  
  comprobacion = c(
    
    "La base de comparación conserva las filas originales",
    
    "La base de comparación conserva el total original",
    
    "La disponibilidad de valores es coherente",
    
    "No existen claves duplicadas dentro de los universos",
    
    "Están presentes los 26 universos esperados",
    
    "Se construyeron los 25 pares consecutivos esperados",
    
    "Las diferencias se calculan solo con valores disponibles",
    
    "No existen valores negativos entre los datos disponibles",
    
    "Los totales recuperados coinciden con los originales",
    
    "La comparación por año de defunción no tiene duplicados"
    
  ),
  
  
  resultado = c(
    
    nrow(datos_comparacion_anual) ==
      nrow(datos_completos),
    
    
    sum(
      datos_comparacion_anual$defunciones,
      na.rm = TRUE
    ) ==
      sum(
        datos_completos$defunciones,
        na.rm = TRUE
      ),
    
    
    !anyNA(datos_comparacion_anual$categoria_disponible) &&
      all(
        datos_comparacion_anual$categoria_disponible ==
          !is.na(datos_comparacion_anual$defunciones)
      ),
    
    
    nrow(duplicados_comparacion) == 0,
    
    
    identical(
      universos_presentes,
      universos_esperados
    ),
    
    
    identical(
      pares_universos_comparados$anio_trabajo_t,
      anios_comparacion
    ) &&
      identical(
        pares_universos_comparados$anio_trabajo_t1,
        anios_comparacion + 1
      ),
    
    
    all(
      is.na(comparaciones_consecutivas$diferencia) |
        (
          comparaciones_consecutivas$presente_t &
            comparaciones_consecutivas$presente_t1 &
            comparaciones_consecutivas$disponible_t &
            comparaciones_consecutivas$disponible_t1
        )
    ),
    
    
    all(
      datos_comparacion_anual$defunciones[
        datos_comparacion_anual$categoria_disponible
      ] >= 0
    ),
    
    
    nrow(validacion_totales_universo) ==
      length(universos_esperados) &&
      all(validacion_totales_universo$coincide),
    
    
    !anyDuplicated(
      comparaciones_por_anio_defuncion %>%
        select(
          anio_trabajo_t,
          anio_trabajo_t1,
          anio_defuncion
        )
    )
    
  )
  
)


# Mostrar resultados.

cat("\n============================================================\n")

cat("VALIDACIONES DE LA SECCIÓN 12\n")

cat("============================================================\n")


print(
  validaciones_seccion_12,
  n = Inf
)


# Comprobar si todas las validaciones fueron superadas.

cat("\n¿Todas las validaciones fueron superadas?\n")

print(
  all(validaciones_seccion_12$resultado)
)


# Detener el script si alguna validación falla.

stopifnot(
  all(validaciones_seccion_12$resultado)
)


cat(
  "\nSección 12 completada: ",
  "comparación de universos consecutivos validada.\n",
  sep = ""
)


# ============================================================
# FIN DE LA SECCIÓN 12
# ============================================================


# ============================================================
# 13. AUDITORÍA DE LA MATRIZ CANDIDATA Y DE LAS
#     COMBINACIONES TEMPORALES AUSENTES
# ============================================================

# OBJETIVO:
#
# 1. Clasificar las combinaciones positivas según la relación
#    entre año de trabajo (universo REDATAM) y año de
#    declaración.
#
# 2. Construir una matriz candidata con el grupo A:
#    año de trabajo = año de declaración.
#
# 3. Examinar la cobertura de retrasos para las defunciones
#    ocurridas entre 2015 y 2025.
#
# 4. Distinguir celdas sin positivos de combinaciones
#    completamente ausentes en datos_completos.
#
# 5. Inspeccionar la cobertura de las combinaciones esperadas
#    en la base procesada.
#
# 6. Presentar descriptivamente los conteos positivos con
#    declaración tardía en los universos 2015–2025.
#
# IMPORTANTE:
#
# - Cada archivo representa un universo independiente.
# - Los universos NO son acumulaciones sucesivas.
# - No se suman universos como si fueran registros
#   individuales independientes.
# - Los conteos agregados no identifican personas únicas.
# - La matriz construida es diagnóstica, no una matriz
#   definitiva para aplicar factores de corrección.
# - El gráfico final describe los conteos de cada universo
#   por separado; no representa defunciones únicas.
#
# ============================================================


# ============================================================
# 13.1. CLASIFICACIÓN SEGÚN AÑO DE TRABAJO Y DECLARACIÓN
# ============================================================


# ------------------------------------------------------------
# 13.1.1. Clasificar las combinaciones positivas
# ------------------------------------------------------------

# Se reutiliza datos_comparacion_anual, construido y validado
# en la sección 12.
#
# Se consideran únicamente registros con conteos positivos.
#
# retraso_real:
#   Diferencia entre año de declaración y año de defunción.
#
# diferencia_trabajo_declaracion:
#   Diferencia entre año del universo REDATAM y año de
#   declaración.
#
# Los grupos permiten distinguir:
#
# A. Año de trabajo = año de declaración.
# B. Año de trabajo posterior al año de declaración.
# C. Declaración posterior al año de trabajo.

auditoria_grupos_trabajo <- datos_comparacion_anual %>%
  
  filter(
    defunciones > 0
  ) %>%
  
  mutate(
    
    retraso_real =
      anio_declaracion - anio_defuncion,
    
    diferencia_trabajo_declaracion =
      anio_trabajo - anio_declaracion,
    
    grupo_trabajo_declaracion = case_when(
      
      anio_trabajo == anio_declaracion ~
        "A. Trabajo = declaración",
      
      anio_trabajo > anio_declaracion ~
        "B. Trabajo posterior a declaración",
      
      anio_trabajo < anio_declaracion ~
        "C. Declaración posterior al trabajo"
      
    )
    
  )


# ------------------------------------------------------------
# 13.1.2. Resumen de los grupos
# ------------------------------------------------------------

# Las sumas representan conteos agregados presentes
# en los archivos correspondientes.
#
# No deben interpretarse como defunciones únicas
# acumuladas entre universos.

resumen_grupos_trabajo <- auditoria_grupos_trabajo %>%
  
  group_by(
    grupo_trabajo_declaracion
  ) %>%
  
  summarise(
    
    combinaciones_positivas = n(),
    
    suma_conteos_archivos = sum(
      defunciones,
      na.rm = TRUE
    ),
    
    universos = n_distinct(anio_trabajo),
    
    anio_trabajo_min = min(anio_trabajo),
    
    anio_trabajo_max = max(anio_trabajo),
    
    anio_declaracion_min = min(anio_declaracion),
    
    anio_declaracion_max = max(anio_declaracion),
    
    .groups = "drop"
    
  ) %>%
  
  arrange(
    grupo_trabajo_declaracion
  )


cat("\nResumen de grupos según año de trabajo y declaración:\n")

print(
  resumen_grupos_trabajo,
  n = Inf
)


# ------------------------------------------------------------
# 13.1.3. Separar los grupos A y B
# ------------------------------------------------------------

grupo_A_trabajo_igual_declaracion <-
  auditoria_grupos_trabajo %>%
  
  filter(
    anio_trabajo == anio_declaracion
  )


grupo_B_trabajo_posterior <-
  auditoria_grupos_trabajo %>%
  
  filter(
    anio_trabajo > anio_declaracion
  )


cat("\nDimensiones del grupo A:\n")

print(
  dim(grupo_A_trabajo_igual_declaracion)
)


cat("\nDimensiones del grupo B:\n")

print(
  dim(grupo_B_trabajo_posterior)
)


# ------------------------------------------------------------
# 13.1.4. Examinar el grupo B
# ------------------------------------------------------------

resumen_grupo_B_declaracion <-
  grupo_B_trabajo_posterior %>%
  
  group_by(
    anio_declaracion
  ) %>%
  
  summarise(
    
    combinaciones_positivas = n(),
    
    suma_conteos_archivos = sum(
      defunciones,
      na.rm = TRUE
    ),
    
    universos_trabajo = n_distinct(anio_trabajo),
    
    anio_trabajo_min = min(anio_trabajo),
    
    anio_trabajo_max = max(anio_trabajo),
    
    .groups = "drop"
    
  ) %>%
  
  arrange(
    anio_declaracion
  )


cat("\nGrupo B: distribución por año de declaración:\n")

print(
  resumen_grupo_B_declaracion,
  n = Inf
)


# ------------------------------------------------------------
# 13.1.5. Construir la matriz candidata del grupo A
# ------------------------------------------------------------

# La matriz organiza los conteos por:
#
# - año de ocurrencia;
# - retraso real de declaración.
#
# Como el grupo A exige que año de trabajo = año de
# declaración, cada combinación año de ocurrencia ×
# retraso determina un único año de trabajo.
#
# El número de universos se calcula antes de seleccionar
# el primer año de trabajo, para que la comprobación
# represente realmente los universos presentes en cada celda.

matriz_candidata_grupo_A <-
  grupo_A_trabajo_igual_declaracion %>%
  
  filter(
    retraso_real >= 0,
    retraso_real <= 9
  ) %>%
  
  group_by(
    anio_defuncion,
    retraso_real
  ) %>%
  
  summarise(
    
    universos_representados =
      n_distinct(anio_trabajo),
    
    anio_trabajo = first(anio_trabajo),
    
    defunciones = sum(
      defunciones,
      na.rm = TRUE
    ),
    
    combinaciones = n(),
    
    .groups = "drop"
    
  ) %>%
  
  arrange(
    anio_defuncion,
    retraso_real
  )


cat("\nDimensiones de la matriz candidata:\n")

print(
  dim(matriz_candidata_grupo_A)
)


cat("\nMatriz candidata del grupo A:\n")

print(
  matriz_candidata_grupo_A,
  n = Inf
)


# ------------------------------------------------------------
# 13.1.6. Resumir la cobertura de la matriz candidata
# ------------------------------------------------------------

cobertura_retrasos_grupo_A <-
  matriz_candidata_grupo_A %>%
  
  group_by(
    anio_defuncion
  ) %>%
  
  summarise(
    
    retrasos_observados = n_distinct(retraso_real),
    
    retraso_min = min(retraso_real),
    
    retraso_max = max(retraso_real),
    
    suma_conteos_archivos = sum(defunciones),
    
    .groups = "drop"
    
  ) %>%
  
  arrange(
    anio_defuncion
  )


cat("\nCobertura de retrasos por año de defunción:\n")

print(
  cobertura_retrasos_grupo_A,
  n = Inf
)


# ------------------------------------------------------------
# 13.1.7. Validaciones de la clasificación y matriz candidata
# ------------------------------------------------------------

validaciones_13_1 <- tibble(
  
  comprobacion = c(
    
    "Todos los positivos están clasificados",
    
    "No existen declaraciones posteriores al año de trabajo",
    
    "El grupo A coincide con trabajo igual a declaración",
    
    "El grupo B coincide con trabajo posterior a declaración",
    
    "Los grupos A y B recuperan todas las combinaciones positivas",
    
    "Los grupos A y B recuperan la suma de los conteos positivos",
    
    "La matriz candidata contiene observaciones",
    
    "La matriz candidata no contiene retrasos negativos",
    
    "La matriz candidata contiene retrasos entre 0 y 9",
    
    "Cada celda candidata procede de un único universo",
    
    "No existen duplicados por año de defunción y retraso"
    
  ),
  
  resultado = c(
    
    !any(is.na(
      auditoria_grupos_trabajo$grupo_trabajo_declaracion
    )),
    
    nrow(
      auditoria_grupos_trabajo %>%
        filter(
          grupo_trabajo_declaracion ==
            "C. Declaración posterior al trabajo"
        )
    ) == 0,
    
    all(
      grupo_A_trabajo_igual_declaracion$anio_trabajo ==
        grupo_A_trabajo_igual_declaracion$anio_declaracion
    ),
    
    all(
      grupo_B_trabajo_posterior$anio_trabajo >
        grupo_B_trabajo_posterior$anio_declaracion
    ),
    
    nrow(grupo_A_trabajo_igual_declaracion) +
      nrow(grupo_B_trabajo_posterior) ==
      nrow(auditoria_grupos_trabajo),
    
    sum(grupo_A_trabajo_igual_declaracion$defunciones) +
      sum(grupo_B_trabajo_posterior$defunciones) ==
      sum(auditoria_grupos_trabajo$defunciones),
    
    nrow(matriz_candidata_grupo_A) > 0,
    
    all(matriz_candidata_grupo_A$retraso_real >= 0),
    
    all(matriz_candidata_grupo_A$retraso_real <= 9),
    
    all(
      matriz_candidata_grupo_A$universos_representados == 1
    ),
    
    !anyDuplicated(
      matriz_candidata_grupo_A %>%
        select(
          anio_defuncion,
          retraso_real
        )
    )
    
  )
  
)


cat("\nValidaciones de la sección 13.1:\n")

print(
  validaciones_13_1,
  n = Inf
)


stopifnot(
  all(validaciones_13_1$resultado)
)


# ============================================================
# 13.2. AUDITORÍA DE COBERTURA Y CELDAS AUSENTES
#      EN LA MATRIZ CANDIDATA 2015–2025
# ============================================================


# ------------------------------------------------------------
# 13.2.1. Construir la cuadrícula de retrasos esperados
# ------------------------------------------------------------

# Se consideran las defunciones ocurridas entre 2015 y 2025.
#
# Se incluyen retrasos desde 0 hasta 9 años, siempre que
# el año de declaración no supere 2025.
#
# Esta cuadrícula representa el alcance temporal definido
# para esta auditoría; no implica que todos los retrasos
# posibles estén observados en los archivos.

cobertura_esperada_2015_2025 <-
  tidyr::expand_grid(
    
    anio_defuncion = 2015:2025,
    
    retraso_real = 0:9
    
  ) %>%
  
  filter(
    
    retraso_real <= pmin(
      9,
      2025 - anio_defuncion
    )
    
  ) %>%
  
  mutate(
    
    anio_declaracion =
      anio_defuncion + retraso_real,
    
    anio_trabajo = anio_declaracion
    
  ) %>%
  
  arrange(
    anio_defuncion,
    retraso_real
  )


cat("\nNúmero de celdas esperadas en la matriz 2015–2025:\n")

print(
  nrow(cobertura_esperada_2015_2025)
)


# ------------------------------------------------------------
# 13.2.2. Incorporar los valores positivos observados
# ------------------------------------------------------------

auditoria_cobertura_matriz <-
  cobertura_esperada_2015_2025 %>%
  
  left_join(
    
    matriz_candidata_grupo_A %>%
      
      select(
        anio_defuncion,
        retraso_real,
        defunciones,
        combinaciones
      ),
    
    by = c(
      "anio_defuncion",
      "retraso_real"
    )
    
  ) %>%
  
  mutate(
    
    celda_positiva_observada =
      !is.na(defunciones),
    
    defunciones_observadas =
      tidyr::replace_na(defunciones, 0),
    
    combinaciones_observadas =
      tidyr::replace_na(combinaciones, 0)
    
  ) %>%
  
  arrange(
    anio_defuncion,
    retraso_real
  )


cat("\nAuditoría de cobertura de la matriz:\n")

print(
  auditoria_cobertura_matriz,
  n = Inf
)


# ------------------------------------------------------------
# 13.2.3. Resumir cobertura por año de ocurrencia
# ------------------------------------------------------------

resumen_cobertura_matriz_2015_2025 <-
  auditoria_cobertura_matriz %>%
  
  group_by(
    anio_defuncion
  ) %>%
  
  summarise(
    
    retrasos_esperados = n(),
    
    retrasos_con_positivos =
      sum(celda_positiva_observada),
    
    retrasos_sin_positivos =
      sum(!celda_positiva_observada),
    
    retrasos_faltantes = paste(
      retraso_real[!celda_positiva_observada],
      collapse = ", "
    ),
    
    suma_conteos_positivos =
      sum(defunciones_observadas),
    
    .groups = "drop"
    
  ) %>%
  
  arrange(
    anio_defuncion
  )


cat("\nResumen de cobertura por año de ocurrencia:\n")

print(
  resumen_cobertura_matriz_2015_2025,
  n = Inf
)


# ------------------------------------------------------------
# 13.2.4. Identificar las celdas sin valores positivos
# ------------------------------------------------------------

celdas_sin_positivos_2015_2025 <-
  auditoria_cobertura_matriz %>%
  
  filter(
    !celda_positiva_observada
  ) %>%
  
  select(
    anio_defuncion,
    retraso_real,
    anio_declaracion,
    anio_trabajo
  ) %>%
  
  arrange(
    anio_defuncion,
    retraso_real
  )


cat("\nCombinaciones sin valores positivos observados:\n")

print(
  celdas_sin_positivos_2015_2025,
  n = Inf
)



# ------------------------------------------------------------
# 13.2.5. EXAMINAR LAS COMBINACIONES EN DATOS_COMPLETOS
# ------------------------------------------------------------

resumen_celdas_base_inec <-
  datos_completos %>%
  group_by(
    universo,
    anio_declaracion,
    anio_defuncion
  ) %>%
  summarise(
    filas_base = n(),
    
    filas_disponibles = sum(
      !is.na(defunciones)
    ),
    
    filas_no_disponibles = sum(
      is.na(defunciones)
    ),
    
    filas_positivas = sum(
      !is.na(defunciones) & defunciones > 0
    ),
    
    suma_defunciones_base = if (
      all(is.na(defunciones))
    ) {
      NA_real_
    } else {
      sum(defunciones, na.rm = TRUE)
    },
    
    .groups = "drop"
  )


auditoria_celdas_base_inec <-
  auditoria_cobertura_matriz %>%
  left_join(
    resumen_celdas_base_inec,
    by = c(
      "anio_trabajo" = "universo",
      "anio_declaracion",
      "anio_defuncion"
    )
  ) %>%
  mutate(
    filas_base = coalesce(filas_base, 0L),
    filas_disponibles = coalesce(filas_disponibles, 0L),
    filas_no_disponibles = coalesce(filas_no_disponibles, 0L),
    filas_positivas = coalesce(filas_positivas, 0L),
    
    suma_defunciones_base = if_else(
      filas_base == 0L,
      0,
      suma_defunciones_base
    ),
    
    estado_celda_base = case_when(
      filas_base == 0L ~
        "Sin filas en datos_completos",
      
      filas_disponibles == 0L ~
        "Filas presentes, sin valores disponibles",
      
      filas_positivas == 0L ~
        "Filas disponibles, sin positivos",
      
      TRUE ~
        "Filas con valores positivos"
    )
  ) %>%
  arrange(
    anio_defuncion,
    retraso_real
  )



# ============================================================
# 13.2.6. EXAMEN DE LAS CELDAS SIN CONTEOS POSITIVOS
# ============================================================

# Identificar las celdas de la matriz candidata que no
# presentan conteos positivos.
#
# Se conserva la distinción entre:
#
# 1. Combinaciones sin filas en datos_completos.
# 2. Combinaciones con filas, pero sin valores disponibles.
# 3. Combinaciones con valores disponibles, pero sin positivos.
#
# Esta clasificación permite distinguir una ausencia de
# registros de una observación con conteo cero.

auditoria_celdas_sin_positivos <-
  auditoria_celdas_base_inec %>%
  
  filter(
    !celda_positiva_observada
  ) %>%
  
  select(
    anio_trabajo,
    anio_declaracion,
    anio_defuncion,
    retraso_real,
    filas_base,
    filas_disponibles,
    filas_no_disponibles,
    filas_positivas,
    suma_defunciones_base,
    estado_celda_base
  ) %>%
  
  arrange(
    anio_defuncion,
    retraso_real
  )


cat("\nAuditoría de las celdas sin conteos positivos:\n")

print(
  auditoria_celdas_sin_positivos,
  n = Inf
)
# ============================================================
# 13.2.7. VALIDACIONES DE LA AUDITORÍA DE COBERTURA
# ============================================================

# Comprobar que las celdas sin positivos están correctamente
# clasificadas en la base INEC.

comprobaciones_auditoria_cobertura <- c(
  
  # Las celdas sin positivos corresponden a celdas sin filas
  # en la base INEC.
  celdas_sin_positivos_sin_filas =
    all(
      auditoria_celdas_sin_positivos$filas_base == 0L
    ),
  
  # No existen filas disponibles en esas celdas.
  celdas_sin_positivos_sin_disponibles =
    all(
      auditoria_celdas_sin_positivos$filas_disponibles == 0L
    ),
  
  # No existen valores positivos en esas celdas.
  celdas_sin_positivos_sin_conteos_positivos =
    all(
      auditoria_celdas_sin_positivos$filas_positivas == 0L
    ),
  
  # La auditoría conserva todas las celdas esperadas.
  auditoria_conserva_celdas =
    nrow(auditoria_celdas_base_inec) ==
    nrow(auditoria_cobertura_matriz),
  
  # No hay duplicados en la clave de la auditoría.
  auditoria_sin_duplicados =
    !anyDuplicated(
      auditoria_celdas_base_inec %>%
        select(
          anio_trabajo,
          anio_declaracion,
          anio_defuncion
        )
    )
)


# Examinar las comprobaciones.

comprobaciones_auditoria_cobertura


# Detener la ejecución si alguna comprobación falla.

stopifnot(
  all(comprobaciones_auditoria_cobertura)
)



# ============================================================
# 13.3. RECUENTO DESCRIPTIVO DE REGISTROS CON DECLARACIÓN
#      TARDÍA EN LOS UNIVERSOS 2015–2025
# ============================================================



# ============================================================
# 13.3.1. RESUMEN DE CONTEOS POSITIVOS POR UNIVERSO Y SEXO
# ============================================================

# OBJETIVO:
#
# Describir las combinaciones con conteos positivos en las que
# el año de declaración es posterior al año de defunción.
#
# Se excluyen explícitamente:
# - Los valores no disponibles.
# - Los conteos iguales a cero.
# - Los conteos negativos.
#
# Cada universo REDATAM se considera independiente.
# Las sumas no representan defunciones únicas.

resumen_tardios_universo_2015_2025 <-
  auditoria_grupos_trabajo %>%
  filter(
    anio_trabajo %in% 2015:2025,
    retraso_real > 0,
    !is.na(defunciones),
    defunciones > 0
  ) %>%
  group_by(
    anio_trabajo,
    sexo
  ) %>%
  summarise(
    combinaciones_positivas = n(),
    
    suma_conteos_tardios = sum(
      defunciones
    ),
    
    .groups = "drop"
  )


# ============================================================
# 13.3.2. COMPLETAR LA CUADRÍCULA DE UNIVERSOS Y SEXOS
# ============================================================

# Construir todas las combinaciones esperadas de año y sexo.

cuadricula_universos_sexo <-
  tidyr::expand_grid(
    anio_trabajo = 2015:2025,
    sexo = sort(unique(datos_completos$sexo))
  )


# Incorporar los resultados del resumen.

resumen_tardios_universo_2015_2025 <-
  cuadricula_universos_sexo %>%
  left_join(
    resumen_tardios_universo_2015_2025,
    by = c(
      "anio_trabajo",
      "sexo"
    )
  ) %>%
  mutate(
    combinaciones_positivas = coalesce(
      combinaciones_positivas,
      0L
    ),
    
    suma_conteos_tardios = coalesce(
      suma_conteos_tardios,
      0
    )
  ) %>%
  arrange(
    anio_trabajo,
    sexo
  )


# Examinar el resultado.

resumen_tardios_universo_2015_2025


# ============================================================
# 13.3.3. VALIDACIONES DEL RESUMEN POR UNIVERSO Y SEXO
# ============================================================

# Reconstruir independientemente los registros que deben
# formar parte del resumen.

datos_positivos_esperados <-
  auditoria_grupos_trabajo %>%
  filter(
    anio_trabajo %in% 2015:2025,
    retraso_real > 0,
    !is.na(defunciones),
    defunciones > 0
  )


# Validaciones.

comprobaciones_resumen_tardios <- c(
  
  # Se conservan los 11 universos esperados.
  universos_esperados =
    identical(
      sort(unique(resumen_tardios_universo_2015_2025$anio_trabajo)),
      2015:2025
    ),
  
  # Se conservan todos los sexos.
  sexos_esperados =
    setequal(
      unique(resumen_tardios_universo_2015_2025$sexo),
      unique(datos_completos$sexo)
    ),
  
  # Existe una sola fila por año y sexo.
  una_fila_por_universo_sexo =
    !anyDuplicated(
      resumen_tardios_universo_2015_2025 %>%
        select(
          anio_trabajo,
          sexo
        )
    ),
  
  # La cuadrícula contiene todas las combinaciones esperadas.
  numero_filas_esperado =
    nrow(resumen_tardios_universo_2015_2025) ==
    11L * n_distinct(datos_completos$sexo),
  
  # El total de combinaciones del resumen coincide con el
  # número de registros positivos seleccionados.
  total_combinaciones_correcto =
    sum(
      resumen_tardios_universo_2015_2025$combinaciones_positivas
    ) ==
    nrow(datos_positivos_esperados),
  
  # La suma de conteos coincide con los registros positivos.
  suma_conteos_correcta =
    isTRUE(
      all.equal(
        sum(
          resumen_tardios_universo_2015_2025$suma_conteos_tardios
        ),
        sum(
          datos_positivos_esperados$defunciones
        )
      )
    ),
  
  # Los conteos de combinaciones no son negativos.
  combinaciones_no_negativas =
    all(
      resumen_tardios_universo_2015_2025$combinaciones_positivas >= 0
    ),
  
  # Las sumas no son negativas.
  sumas_no_negativas =
    all(
      resumen_tardios_universo_2015_2025$suma_conteos_tardios >= 0
    )
)


# Examinar los resultados.

comprobaciones_resumen_tardios


# Detener la ejecución si alguna comprobación falla.

stopifnot(
  all(comprobaciones_resumen_tardios)
)


# ------------------------------------------------------------
# 13.3.4. Representación gráfica
# ------------------------------------------------------------

# Cada barra representa un universo REDATAM independiente.
#
# Se presentan paneles separados por sexo.
#
# El eje Y representa la suma de los conteos positivos
# correspondientes a declaraciones posteriores al año
# de defunción.
#
# No se trata de defunciones únicas ni de un total acumulado
# entre universos.

grafico_tardios_universo_2015_2025 <-
  ggplot(
    resumen_tardios_universo_2015_2025,
    aes(
      x = anio_trabajo,
      y = suma_conteos_tardios
    )
  ) +
  
  geom_col(
    width = 0.75
  ) +
  
  facet_wrap(
    ~ sexo,
    ncol = 1
  ) +
  
  scale_x_continuous(
    breaks = 2015:2025,
    limits = c(2014.5, 2025.5),
    expand = c(0, 0)
  ) +
  
  scale_y_continuous(
    expand = expansion(
      mult = c(0, 0.08)
    )
  ) +
  
  labs(
    title = "Conteos positivos de defunciones declaradas después del año de ocurrencia",
    subtitle = "Universos REDATAM independientes, 2015–2025",
    caption = paste(
      "El desfase se define como año de declaración menos año de defunción.",
      "Cada barra corresponde a un universo anual independiente.",
      "Los conteos agregados no representan defunciones únicas",
      "ni deben sumarse entre universos."
    )
  ) +
  
  theme_minimal() +
  
  theme(
    plot.title = element_text(
      face = "bold"
    ),
    strip.text = element_text(
      face = "bold"
    ),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(
      hjust = 0
    )
  )


print(
  grafico_tardios_universo_2015_2025
)


# ============================================================
# 13.4. CIERRE DE LA SECCIÓN
# ============================================================

cat(
  "\nSECCIÓN 13 COMPLETADA:",
  "\nClasificación temporal, matriz candidata,",
  "\nauditoría de cobertura y resumen descriptivo",
  "\nde los conteos positivos por universo validados.\n"
)


# ============================================================
# 14.1. AUDITORÍA DE LA RELACIÓN ENTRE UNIVERSO REDATAM
#       Y AÑO DE DECLARACIÓN
# ============================================================

# OBJETIVO:
#
# Examinar cómo se relacionan:
# 1. El año del universo REDATAM.
# 2. El año de declaración.
# 3. El año de ocurrencia.
#
# Se utilizan únicamente combinaciones con defunciones
# positivas de los universos 2015-2025.
#
# IMPORTANTE:
# Los universos son archivos anuales independientes.
# Esta auditoría no supone que sean fotografías acumulativas
# ni que sus conteos representen defunciones únicas.
#
# Tampoco establece todavía qué registros deben utilizarse
# para construir la matriz definitiva de registro tardío.


# ------------------------------------------------------------
# 14.1.1. Seleccionar las combinaciones positivas
# ------------------------------------------------------------

auditoria_relacion_universo <- auditoria_grupos_trabajo %>%
  filter(
    anio_trabajo %in% 2015:2025,
    defunciones > 0
  ) %>%
  mutate(
    relacion_universo_declaracion = case_when(
      anio_trabajo == anio_declaracion ~
        "Universo igual al año de declaración",
      
      anio_trabajo > anio_declaracion ~
        "Universo posterior al año de declaración",
      
      anio_trabajo < anio_declaracion ~
        "Universo anterior al año de declaración",
      
      TRUE ~ NA_character_
    )
  )


# ------------------------------------------------------------
# 14.1.2. Resumir la relación por universo
# ------------------------------------------------------------

resumen_relacion_universo <- auditoria_relacion_universo %>%
  group_by(
    anio_trabajo,
    relacion_universo_declaracion
  ) %>%
  summarise(
    combinaciones_positivas = n(),
    
    suma_defunciones = sum(defunciones),
    
    anios_ocurrencia = n_distinct(anio_defuncion),
    
    anios_declaracion = n_distinct(anio_declaracion),
    
    .groups = "drop"
  ) %>%
  arrange(
    anio_trabajo,
    relacion_universo_declaracion
  )


# ------------------------------------------------------------
# 14.1.3. Resumir la relación entre universo y declaración
# ------------------------------------------------------------

resumen_universo_declaracion <- auditoria_relacion_universo %>%
  group_by(
    anio_trabajo,
    anio_declaracion
  ) %>%
  summarise(
    combinaciones_positivas = n(),
    
    suma_defunciones = sum(defunciones),
    
    anios_ocurrencia = n_distinct(anio_defuncion),
    
    ocurrencia_minima = min(anio_defuncion),
    
    ocurrencia_maxima = max(anio_defuncion),
    
    .groups = "drop"
  ) %>%
  arrange(
    anio_trabajo,
    anio_declaracion
  )


# ------------------------------------------------------------
# 14.1.4. Examinar las combinaciones por universo,
#         declaración y ocurrencia
# ------------------------------------------------------------

resumen_triple_anual <- auditoria_relacion_universo %>%
  group_by(
    anio_trabajo,
    anio_declaracion,
    anio_defuncion
  ) %>%
  summarise(
    combinaciones_positivas = n(),
    
    suma_defunciones = sum(defunciones),
    
    .groups = "drop"
  ) %>%
  arrange(
    anio_trabajo,
    anio_declaracion,
    anio_defuncion
  )


# ------------------------------------------------------------
# 14.1.5. Resumen global de las relaciones temporales
# ------------------------------------------------------------

resumen_global_relacion <- auditoria_relacion_universo %>%
  group_by(relacion_universo_declaracion) %>%
  summarise(
    combinaciones_positivas = n(),
    
    suma_defunciones = sum(defunciones),
    
    universos = n_distinct(anio_trabajo),
    
    .groups = "drop"
  ) %>%
  arrange(relacion_universo_declaracion)


# ------------------------------------------------------------
# 14.1.6. Validaciones
# ------------------------------------------------------------

validaciones_14_1 <- tibble(
  validacion = c(
    "Todos los registros corresponden a universos 2015-2025",
    "Todas las combinaciones tienen defunciones positivas",
    "No hay relaciones temporales sin clasificar",
    "Las categorías reconstruyen el número de combinaciones",
    "Las categorías reconstruyen la suma de conteos"
  ),
  
  resultado = c(
    all(auditoria_relacion_universo$anio_trabajo %in% 2015:2025),
    
    all(auditoria_relacion_universo$defunciones > 0),
    
    !anyNA(
      auditoria_relacion_universo$relacion_universo_declaracion
    ),
    
    sum(resumen_relacion_universo$combinaciones_positivas) ==
      nrow(auditoria_relacion_universo),
    
    sum(resumen_relacion_universo$suma_defunciones) ==
      sum(auditoria_relacion_universo$defunciones)
  )
)

print(validaciones_14_1)

stopifnot(
  all(validaciones_14_1$resultado)
)


# ------------------------------------------------------------
# 14.1.7. Mostrar resultados
# ------------------------------------------------------------

cat("\nRESUMEN GLOBAL DE RELACIONES TEMPORALES\n")

print(resumen_global_relacion, n = Inf)

cat("\nRELACIÓN ENTRE UNIVERSO Y DECLARACIÓN\n")

print(resumen_relacion_universo, n = Inf)

cat("\nDETALLE DE UNIVERSO Y AÑO DE DECLARACIÓN\n")

print(resumen_universo_declaracion, n = Inf)

cat("\nDETALLE DE UNIVERSO, DECLARACIÓN Y OCURRENCIA\n")

print(resumen_triple_anual, n = 60)

cat("\nSECCIÓN 14.1 COMPLETADA.\n")


# ============================================================
# 14.2. COMPARACIÓN DE LOS REGISTROS DEL GRUPO B
#      CON SU UNIVERSO DE REFERENCIA
# ============================================================

# OBJETIVO:
#
# Comparar las combinaciones positivas de los universos
# 2015-2025 cuyo año de universo es posterior al año
# de declaración (grupo B), con las combinaciones
# correspondientes del universo cuyo año coincide
# con el año de declaración (grupo A).
#
# La comparación permite identificar si los registros
# del grupo B:
#
# 1. Ya estaban presentes con el mismo conteo.
# 2. Estaban presentes con un conteo diferente.
# 3. No estaban presentes en el universo de referencia.
#
# Se utiliza provincia_id como identificador territorial.
#
# IMPORTANTE:
#
# Esta comparación es diagnóstica.
# No presupone que los universos sean fotografías
# acumulativas ni que las diferencias representen
# defunciones adicionales o registros tardíos nuevos.


# ------------------------------------------------------------
# 14.2.1. Construir la base de referencia del grupo A
# ------------------------------------------------------------

base_referencia_grupo_A <- auditoria_grupos_trabajo %>%
  filter(
    anio_trabajo == anio_declaracion,
    defunciones > 0
  ) %>%
  transmute(
    anio_declaracion,
    anio_defuncion,
    sexo,
    provincia_id,
    edad_quinquenal,
    
    universo_referencia = anio_trabajo,
    
    defunciones_referencia = defunciones
  )


# ------------------------------------------------------------
# 14.2.2. Seleccionar los registros del grupo B
# ------------------------------------------------------------

base_grupo_B <- auditoria_grupos_trabajo %>%
  filter(
    anio_trabajo %in% 2015:2025,
    anio_trabajo > anio_declaracion,
    defunciones > 0
  ) %>%
  transmute(
    anio_trabajo,
    anio_declaracion,
    anio_defuncion,
    sexo,
    provincia_id,
    edad_quinquenal,
    
    defunciones_grupo_B = defunciones
  )


# ------------------------------------------------------------
# 14.2.3. Comparar cada registro del grupo B con
#         su universo de referencia
# ------------------------------------------------------------

comparacion_grupo_B_referencia <- base_grupo_B %>%
  left_join(
    base_referencia_grupo_A,
    by = c(
      "anio_declaracion",
      "anio_defuncion",
      "sexo",
      "provincia_id",
      "edad_quinquenal"
    )
  ) %>%
  mutate(
    estado_comparacion = case_when(
      
      is.na(defunciones_referencia) ~
        "Sin coincidencia en universo de referencia",
      
      defunciones_grupo_B == defunciones_referencia ~
        "Coincidente e idéntica",
      
      TRUE ~
        "Coincidente con diferente conteo"
    ),
    
    diferencia_conteos = if_else(
      !is.na(defunciones_referencia),
      defunciones_grupo_B - defunciones_referencia,
      NA_real_
    )
  )


# ------------------------------------------------------------
# 14.2.4. Resumen global de la comparación
# ------------------------------------------------------------

resumen_global_grupo_B <- comparacion_grupo_B_referencia %>%
  group_by(estado_comparacion) %>%
  summarise(
    combinaciones = n(),
    
    suma_grupo_B = sum(defunciones_grupo_B),
    
    suma_referencia = sum(
      defunciones_referencia,
      na.rm = TRUE
    ),
    
    universos = n_distinct(anio_trabajo),
    
    .groups = "drop"
  ) %>%
  arrange(estado_comparacion)


# ------------------------------------------------------------
# 14.2.5. Resumen por universo REDATAM
# ------------------------------------------------------------

resumen_grupo_B_por_universo <- comparacion_grupo_B_referencia %>%
  group_by(
    anio_trabajo,
    anio_declaracion,
    estado_comparacion
  ) %>%
  summarise(
    combinaciones = n(),
    
    suma_grupo_B = sum(defunciones_grupo_B),
    
    suma_referencia = sum(
      defunciones_referencia,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  ) %>%
  arrange(
    anio_trabajo,
    anio_declaracion,
    estado_comparacion
  )


# ------------------------------------------------------------
# 14.2.6. Examinar los registros sin coincidencia
# ------------------------------------------------------------

detalle_sin_coincidencia_grupo_B <-
  comparacion_grupo_B_referencia %>%
  filter(
    estado_comparacion ==
      "Sin coincidencia en universo de referencia"
  ) %>%
  arrange(
    anio_trabajo,
    anio_declaracion,
    anio_defuncion,
    sexo,
    provincia_id,
    edad_quinquenal
  )


# ------------------------------------------------------------
# 14.2.7. Examinar los registros con conteos diferentes
# ------------------------------------------------------------

detalle_conteos_diferentes_grupo_B <-
  comparacion_grupo_B_referencia %>%
  filter(
    estado_comparacion ==
      "Coincidente con diferente conteo"
  ) %>%
  arrange(
    anio_trabajo,
    anio_declaracion,
    anio_defuncion,
    sexo,
    provincia_id,
    edad_quinquenal
  )


# ------------------------------------------------------------
# 14.2.8. Validaciones
# ------------------------------------------------------------

validaciones_14_2 <- tibble(
  validacion = c(
    "La base de referencia no contiene claves duplicadas",
    "La base del grupo B no contiene claves duplicadas por universo",
    "Todos los registros del grupo B corresponden a universos 2015-2025",
    "Todos los registros seleccionados del grupo B tienen conteos positivos",
    "Todos los registros del grupo B tienen un estado de comparación",
    "Las categorías reconstruyen el número de registros del grupo B"
  ),
  
  resultado = c(
    
    !anyDuplicated(
      base_referencia_grupo_A %>%
        select(
          anio_declaracion,
          anio_defuncion,
          sexo,
          provincia_id,
          edad_quinquenal
        )
    ),
    
    !anyDuplicated(
      base_grupo_B %>%
        select(
          anio_trabajo,
          anio_declaracion,
          anio_defuncion,
          sexo,
          provincia_id,
          edad_quinquenal
        )
    ),
    
    all(
      base_grupo_B$anio_trabajo %in% 2015:2025
    ),
    
    all(
      base_grupo_B$defunciones_grupo_B > 0
    ),
    
    !anyNA(
      comparacion_grupo_B_referencia$estado_comparacion
    ),
    
    sum(
      resumen_global_grupo_B$combinaciones
    ) == nrow(base_grupo_B)
  )
)

print(validaciones_14_2)

stopifnot(
  all(validaciones_14_2$resultado)
)


# ------------------------------------------------------------
# 14.2.9. Mostrar resultados
# ------------------------------------------------------------

cat("\nRESUMEN GLOBAL DE COMPARACIÓN DEL GRUPO B\n")

print(resumen_global_grupo_B, n = Inf)

cat("\nCOMPARACIÓN POR UNIVERSO Y AÑO DE DECLARACIÓN\n")

print(resumen_grupo_B_por_universo, n = Inf)

cat("\nREGISTROS SIN COINCIDENCIA\n")

print(
  detalle_sin_coincidencia_grupo_B,
  n = Inf
)

cat("\nREGISTROS CON CONTEOS DIFERENTES\n")

print(
  detalle_conteos_diferentes_grupo_B,
  n = Inf
)

cat("\nSECCIÓN 14.2 COMPLETADA.\n")


names(comparacion_grupo_B_referencia)

dplyr::glimpse(comparacion_grupo_B_referencia)


# ============================================================
# 14.3. ANÁLISIS DE MAGNITUD Y DIRECCIÓN DE LAS DIFERENCIAS
#       ENTRE EL GRUPO B Y SU UNIVERSO DE REFERENCIA
# ============================================================

# OBJETIVO:
#
# Examinar la magnitud y dirección de las diferencias entre
# los conteos positivos del grupo B y los conteos de las mismas
# claves en el universo de referencia.
#
# Se busca:
#
# 1. Cuantificar la dirección de las diferencias.
# 2. Distinguir diferencias de una unidad y superiores.
# 3. Describir su distribución por universo de trabajo.
# 4. Describir su distribución por año de declaración.
# 5. Examinar su distribución por provincia, sexo y edad.
# 6. Identificar las combinaciones con mayores diferencias.
#
# IMPORTANTE:
#
# Las diferencias corresponden a conteos agregados.
# No demuestran identidad de defunciones individuales ni
# permiten determinar si existen incorporaciones nuevas,
# subconjuntos o una combinación de ambos procesos.
#
# No se estiman factores de corrección.
#
# ============================================================


# ------------------------------------------------------------
# 14.3.1. CONSTRUIR LA BASE DE DIFERENCIAS COINCIDENTES
# ------------------------------------------------------------

diferencias_grupo_B <- comparacion_grupo_B_referencia %>%
  filter(
    estado_comparacion !=
      "Sin coincidencia en universo de referencia"
  ) %>%
  mutate(
    diferencia_absoluta = abs(diferencia_conteos),
    
    direccion_diferencia = case_when(
      diferencia_conteos < 0 ~
        "Grupo B menor que referencia",
      
      diferencia_conteos > 0 ~
        "Grupo B mayor que referencia",
      
      diferencia_conteos == 0 ~
        "Conteos iguales",
      
      TRUE ~ NA_character_
    ),
    
    magnitud_diferencia = case_when(
      diferencia_absoluta == 0 ~
        "Sin diferencia",
      
      diferencia_absoluta == 1 ~
        "Diferencia de una unidad",
      
      diferencia_absoluta > 1 ~
        "Diferencia superior a una unidad",
      
      TRUE ~ NA_character_
    )
  )


# ------------------------------------------------------------
# 14.3.2. RESUMEN GLOBAL DE DIRECCIÓN DE LAS DIFERENCIAS
# ------------------------------------------------------------

resumen_direccion_diferencias <- diferencias_grupo_B %>%
  count(
    direccion_diferencia,
    name = "combinaciones"
  ) %>%
  mutate(
    total_combinaciones = sum(combinaciones),
    porcentaje = 100 * combinaciones / total_combinaciones
  ) %>%
  arrange(
    factor(
      direccion_diferencia,
      levels = c(
        "Grupo B menor que referencia",
        "Grupo B mayor que referencia",
        "Conteos iguales"
      )
    )
  )


# ------------------------------------------------------------
# 14.3.3. RESUMEN GLOBAL DE MAGNITUD DE LAS DIFERENCIAS
# ------------------------------------------------------------

resumen_magnitud_diferencias <- diferencias_grupo_B %>%
  count(
    magnitud_diferencia,
    name = "combinaciones"
  ) %>%
  mutate(
    total_combinaciones = sum(combinaciones),
    porcentaje = 100 * combinaciones / total_combinaciones
  ) %>%
  arrange(
    factor(
      magnitud_diferencia,
      levels = c(
        "Sin diferencia",
        "Diferencia de una unidad",
        "Diferencia superior a una unidad"
      )
    )
  )


# ------------------------------------------------------------
# 14.3.4. RESUMEN POR UNIVERSO DE TRABAJO
# ------------------------------------------------------------

resumen_diferencias_por_universo <- diferencias_grupo_B %>%
  group_by(anio_trabajo) %>%
  summarise(
    combinaciones = n(),
    
    grupo_B_menor = sum(diferencia_conteos < 0),
    grupo_B_mayor = sum(diferencia_conteos > 0),
    conteos_iguales = sum(diferencia_conteos == 0),
    
    diferencia_absoluta_total = sum(diferencia_absoluta),
    diferencia_absoluta_mediana = median(diferencia_absoluta),
    diferencia_absoluta_maxima = max(diferencia_absoluta),
    
    .groups = "drop"
  ) %>%
  arrange(anio_trabajo)


# ------------------------------------------------------------
# 14.3.5. RESUMEN POR AÑO DE DECLARACIÓN
# ------------------------------------------------------------

resumen_diferencias_por_declaracion <- diferencias_grupo_B %>%
  group_by(anio_declaracion) %>%
  summarise(
    combinaciones = n(),
    
    grupo_B_menor = sum(diferencia_conteos < 0),
    grupo_B_mayor = sum(diferencia_conteos > 0),
    conteos_iguales = sum(diferencia_conteos == 0),
    
    diferencia_absoluta_total = sum(diferencia_absoluta),
    diferencia_absoluta_mediana = median(diferencia_absoluta),
    diferencia_absoluta_maxima = max(diferencia_absoluta),
    
    .groups = "drop"
  ) %>%
  arrange(anio_declaracion)


# ------------------------------------------------------------
# 14.3.6. RESUMEN POR PROVINCIA
# ------------------------------------------------------------

resumen_diferencias_por_provincia <- diferencias_grupo_B %>%
  group_by(provincia_id) %>%
  summarise(
    combinaciones = n(),
    
    grupo_B_menor = sum(diferencia_conteos < 0),
    grupo_B_mayor = sum(diferencia_conteos > 0),
    conteos_iguales = sum(diferencia_conteos == 0),
    
    diferencia_absoluta_total = sum(diferencia_absoluta),
    diferencia_absoluta_mediana = median(diferencia_absoluta),
    
    .groups = "drop"
  ) %>%
  arrange(provincia_id)


# ------------------------------------------------------------
# 14.3.7. RESUMEN POR SEXO
# ------------------------------------------------------------

resumen_diferencias_por_sexo <- diferencias_grupo_B %>%
  group_by(sexo) %>%
  summarise(
    combinaciones = n(),
    
    grupo_B_menor = sum(diferencia_conteos < 0),
    grupo_B_mayor = sum(diferencia_conteos > 0),
    conteos_iguales = sum(diferencia_conteos == 0),
    
    diferencia_absoluta_total = sum(diferencia_absoluta),
    diferencia_absoluta_mediana = median(diferencia_absoluta),
    
    .groups = "drop"
  )


# ------------------------------------------------------------
# 14.3.8. RESUMEN POR EDAD
# ------------------------------------------------------------

resumen_diferencias_por_edad <- diferencias_grupo_B %>%
  group_by(edad_quinquenal) %>%
  summarise(
    combinaciones = n(),
    
    grupo_B_menor = sum(diferencia_conteos < 0),
    grupo_B_mayor = sum(diferencia_conteos > 0),
    conteos_iguales = sum(diferencia_conteos == 0),
    
    diferencia_absoluta_total = sum(diferencia_absoluta),
    diferencia_absoluta_mediana = median(diferencia_absoluta),
    
    .groups = "drop"
  )


# ------------------------------------------------------------
# 14.3.9. DETALLE DE LAS MAYORES DIFERENCIAS
# ------------------------------------------------------------

detalle_mayores_diferencias <- diferencias_grupo_B %>%
  arrange(
    desc(diferencia_absoluta),
    anio_trabajo,
    anio_declaracion
  ) %>%
  select(
    anio_trabajo,
    anio_declaracion,
    anio_defuncion,
    sexo,
    provincia_id,
    edad_quinquenal,
    defunciones_grupo_B,
    defunciones_referencia,
    diferencia_conteos,
    diferencia_absoluta,
    direccion_diferencia
  )


# ------------------------------------------------------------
# 14.3.10. VALIDACIONES
# ------------------------------------------------------------

comprobaciones_seccion_14_3 <- c(
  
  solo_claves_coincidentes =
    all(
      diferencias_grupo_B$estado_comparacion !=
        "Sin coincidencia en universo de referencia"
    ),
  
  sin_diferencias_faltantes =
    !anyNA(diferencias_grupo_B$diferencia_conteos),
  
  diferencia_calculada_correctamente =
    all(
      diferencias_grupo_B$diferencia_conteos ==
        diferencias_grupo_B$defunciones_grupo_B -
        diferencias_grupo_B$defunciones_referencia
    ),
  
  diferencia_absoluta_correcta =
    all(
      diferencias_grupo_B$diferencia_absoluta ==
        abs(diferencias_grupo_B$diferencia_conteos)
    ),
  
  categorias_direccion_reconstruyen_total =
    sum(resumen_direccion_diferencias$combinaciones) ==
    nrow(diferencias_grupo_B),
  
  categorias_magnitud_reconstruyen_total =
    sum(resumen_magnitud_diferencias$combinaciones) ==
    nrow(diferencias_grupo_B),
  
  resumen_universos_reconstruye_total =
    sum(resumen_diferencias_por_universo$combinaciones) ==
    nrow(diferencias_grupo_B),
  
  resumen_declaraciones_reconstruye_total =
    sum(resumen_diferencias_por_declaracion$combinaciones) ==
    nrow(diferencias_grupo_B),
  
  diferencias_absolutas_no_negativas =
    all(diferencias_grupo_B$diferencia_absoluta >= 0),
  
  numero_claves_coincidentes_correcto =
    nrow(diferencias_grupo_B) ==
    sum(
      resumen_global_grupo_B$combinaciones[
        resumen_global_grupo_B$estado_comparacion !=
          "Sin coincidencia en universo de referencia"
      ]
    )
)


print(comprobaciones_seccion_14_3)

cat(
  "\nValidaciones superadas:",
  sum(comprobaciones_seccion_14_3),
  "de",
  length(comprobaciones_seccion_14_3),
  "\n"
)

stopifnot(all(comprobaciones_seccion_14_3))


# ------------------------------------------------------------
# 14.3.11. PRESENTACIÓN DE RESULTADOS
# ------------------------------------------------------------

cat("\nDIRECCIÓN DE LAS DIFERENCIAS\n")
print(resumen_direccion_diferencias)

cat("\nMAGNITUD DE LAS DIFERENCIAS\n")
print(resumen_magnitud_diferencias)

cat("\nDIFERENCIAS POR UNIVERSO\n")
print(resumen_diferencias_por_universo)

cat("\nDIFERENCIAS POR AÑO DE DECLARACIÓN\n")
print(resumen_diferencias_por_declaracion)

cat("\nDIFERENCIAS POR PROVINCIA\n")
print(resumen_diferencias_por_provincia)

cat("\nDIFERENCIAS POR SEXO\n")
print(resumen_diferencias_por_sexo)

cat("\nDIFERENCIAS POR EDAD\n")
print(resumen_diferencias_por_edad)

cat("\n20 MAYORES DIFERENCIAS\n")
print(
  detalle_mayores_diferencias %>%
    slice_head(n = 20)
)


# ------------------------------------------------------------
# 14.3.12. CIERRE
# ------------------------------------------------------------

cat(
  "\nSECCIÓN 14.3 COMPLETADA.",
  "\nSe examinó la dirección y magnitud de las diferencias",
  "\nentre los conteos del grupo B y sus universos de referencia.",
  "\nLos resultados son descriptivos y no constituyen una",
  "\nestimación de defunciones tardías adicionales ni un",
  "\nprocedimiento de corrección.\n"
)



# ============================================================
# 14.4. RECONSTRUCCIÓN DE LA MATRIZ DE OCURRENCIA Y DECLARACIÓN
# ============================================================

# OBJETIVO:
#
# Reconstruir la matriz de defunciones por año de ocurrencia
# y año de declaración, utilizando como fuente de referencia
# el universo REDATAM cuyo año coincide con el año de
# declaración.
#
# Esta matriz servirá para:
#
# 1. Reproducir el indicador de oportunidad del registro
#    utilizado en el ejemplo CEPAL/Brasil.
#
# 2. Preparar la matriz de rezagos requerida por el método
#    de corrección del INEC.
#
# CRITERIO:
#
# Cada año de declaración se obtiene de su universo de
# referencia:
#
# universo == anio_declaracion
#
# Los demás universos no se suman a esta matriz.
# Se reservan para auditorías de consistencia.
#
# Se conservan los registros con cero y los registros
# no disponibles.
#
# ============================================================


# ------------------------------------------------------------
# 14.4.1. SELECCIONAR LOS REGISTROS DE LOS UNIVERSOS
#        DE REFERENCIA
# ------------------------------------------------------------

matriz_referencia_declaracion <- datos_completos %>%
  filter(
    universo == anio_declaracion,
    anio_declaracion >= 2015,
    anio_declaracion <= 2025
  ) %>%
  select(
    universo,
    sexo,
    provincia_id,
    provincia,
    anio_declaracion,
    anio_defuncion,
    edad_quinquenal,
    categoria_disponible,
    valor_original,
    estado_registro,
    defunciones,
    retraso_declaracion
  )


# ------------------------------------------------------------
# 14.4.2. RESUMIR LA COBERTURA DE LOS UNIVERSOS
# ------------------------------------------------------------

resumen_cobertura_referencia <- matriz_referencia_declaracion %>%
  group_by(universo, anio_declaracion) %>%
  summarise(
    filas = n(),
    
    filas_disponibles = sum(categoria_disponible),
    
    filas_no_disponibles = sum(!categoria_disponible),
    
    filas_positivas = sum(
      categoria_disponible & defunciones > 0
    ),
    
    filas_guion = sum(
      categoria_disponible &
        estado_registro == "guion"
    ),
    
    total_defunciones = sum(
      defunciones[categoria_disponible],
      na.rm = TRUE
    ),
    
    .groups = "drop"
  ) %>%
  arrange(anio_declaracion)


# ------------------------------------------------------------
# 14.4.3. CONSTRUIR LA MATRIZ AGREGADA
#        AÑO DE OCURRENCIA × AÑO DE DECLARACIÓN
# ------------------------------------------------------------

matriz_ocurrencia_declaracion <- matriz_referencia_declaracion %>%
  group_by(
    anio_defuncion,
    anio_declaracion
  ) %>%
  summarise(
    filas_base = n(),
    filas_disponibles = sum(categoria_disponible),
    filas_no_disponibles = sum(!categoria_disponible),
    
    defunciones = sum(
      defunciones[categoria_disponible],
      na.rm = TRUE
    ),
    
    .groups = "drop"
  ) %>%
  mutate(
    retraso = anio_declaracion - anio_defuncion
  ) %>%
  arrange(
    anio_defuncion,
    anio_declaracion
  )


# ------------------------------------------------------------
# 14.4.4. CLASIFICAR LAS COMBINACIONES TEMPORALES
# ------------------------------------------------------------

matriz_ocurrencia_declaracion <- matriz_ocurrencia_declaracion %>%
  mutate(
    tipo_registro = case_when(
      retraso < 0 ~ "Anomalía temporal",
      retraso == 0 ~ "Registro oportuno",
      retraso > 0 ~ "Registro tardío"
    ),
    
    estado_disponibilidad = case_when(
      filas_disponibles == 0 ~
        "Sin valores disponibles",
      
      filas_no_disponibles > 0 ~
        "Disponibilidad parcial",
      
      TRUE ~
        "Valores disponibles"
    )
  )


# ------------------------------------------------------------
# 14.4.5. AUDITAR LA VENTANA DE REZAGOS 0–9
# ------------------------------------------------------------

matriz_rezagos_inec <- matriz_ocurrencia_declaracion %>%
  filter(
    retraso >= 0,
    retraso <= 9
  ) %>%
  arrange(
    anio_defuncion,
    retraso
  )


# ------------------------------------------------------------
# 14.4.6. RESUMIR LA MATRIZ POR AÑO DE OCURRENCIA
# ------------------------------------------------------------

resumen_matriz_ocurrencia <- matriz_rezagos_inec %>%
  group_by(anio_defuncion) %>%
  summarise(
    numero_rezagos_observados = n_distinct(retraso),
    
    numero_rezagos_con_registros =
      sum(filas_disponibles > 0),
    
    numero_rezagos_no_disponibles =
      sum(filas_disponibles == 0),
    
    numero_rezagos_con_defunciones_positivas =
      sum(defunciones > 0),
    
    total_defunciones_observadas =
      sum(defunciones),
    
    .groups = "drop"
  ) %>%
  arrange(anio_defuncion)


# ------------------------------------------------------------
# 14.4.7. VALIDACIONES
# ------------------------------------------------------------

comprobaciones_seccion_14_4 <- c(
  
  # Cada registro procede de su universo de referencia.
  universo_corresponde_declaracion =
    all(
      matriz_referencia_declaracion$universo ==
        matriz_referencia_declaracion$anio_declaracion
    ),
  
  # No hay duplicados en la clave original.
  clave_original_sin_duplicados =
    !anyDuplicated(
      matriz_referencia_declaracion %>%
        select(
          universo,
          sexo,
          provincia_id,
          anio_declaracion,
          anio_defuncion,
          edad_quinquenal
        )
    ),
  
  # La matriz agregada no tiene duplicados temporales.
  matriz_temporal_sin_duplicados =
    !anyDuplicated(
      matriz_ocurrencia_declaracion %>%
        select(
          anio_defuncion,
          anio_declaracion
        )
    ),
  
  # El retraso está correctamente calculado.
  retraso_correcto =
    all(
      matriz_ocurrencia_declaracion$retraso ==
        matriz_ocurrencia_declaracion$anio_declaracion -
        matriz_ocurrencia_declaracion$anio_defuncion
    ),
  
  # No se han incorporado retrasos negativos a la matriz INEC.
  ventana_rezagos_correcta =
    all(
      matriz_rezagos_inec$retraso >= 0 &
        matriz_rezagos_inec$retraso <= 9
    ),
  
  # La matriz conserva las filas disponibles y no disponibles.
  filas_reconstruidas =
    all(
      matriz_ocurrencia_declaracion$filas_base ==
        matriz_ocurrencia_declaracion$filas_disponibles +
        matriz_ocurrencia_declaracion$filas_no_disponibles
    ),
  
  # La suma agregada reconstruye los registros disponibles.
  totales_reconstruidos =
    sum(matriz_ocurrencia_declaracion$defunciones) ==
    sum(
      matriz_referencia_declaracion$defunciones[
        matriz_referencia_declaracion$categoria_disponible
      ],
      na.rm = TRUE
    )
)


print(comprobaciones_seccion_14_4)

cat(
  "\nValidaciones superadas:",
  sum(comprobaciones_seccion_14_4),
  "de",
  length(comprobaciones_seccion_14_4),
  "\n"
)

stopifnot(all(comprobaciones_seccion_14_4))


# ------------------------------------------------------------
# 14.4.8. PRESENTAR LOS RESULTADOS
# ------------------------------------------------------------

cat("\nCOBERTURA DE UNIVERSOS DE REFERENCIA\n")

print(
  resumen_cobertura_referencia,
  n = Inf,
  width = Inf
)


cat("\nMATRIZ DE OCURRENCIA Y DECLARACIÓN\n")

print(
  matriz_ocurrencia_declaracion,
  n = Inf,
  width = Inf
)


cat("\nMATRIZ DE REZAGOS 0–9\n")

print(
  matriz_rezagos_inec,
  n = Inf,
  width = Inf
)


cat("\nRESUMEN DE COBERTURA POR AÑO DE OCURRENCIA\n")

print(
  resumen_matriz_ocurrencia,
  n = Inf,
  width = Inf
)


# ============================================================
# 14.5. AUDITORÍA DE LA COBERTURA DE LA MATRIZ TEMPORAL
# ============================================================

# OBJETIVO:
#
# Construir todas las combinaciones esperadas de año de
# ocurrencia y rezago 0–9, limitadas al año de declaración
# 2025.
#
# Identificar las combinaciones ausentes sin convertirlas
# automáticamente en ceros.
#
# ============================================================


# ------------------------------------------------------------
# 14.5.1. CONSTRUIR LA CUADRÍCULA TEMPORAL ESPERADA
# ------------------------------------------------------------

cuadricula_rezagos_esperados <- tidyr::crossing(
  anio_defuncion = 2015:2025,
  retraso = 0:9
) %>%
  mutate(
    anio_declaracion =
      anio_defuncion + retraso
  ) %>%
  filter(
    anio_declaracion <= 2025
  ) %>%
  arrange(
    anio_defuncion,
    retraso
  )


# ------------------------------------------------------------
# 14.5.2. INCORPORAR LAS COMBINACIONES OBSERVADAS
# ------------------------------------------------------------

auditoria_cobertura_rezagos <- cuadricula_rezagos_esperados %>%
  left_join(
    matriz_ocurrencia_declaracion %>%
      select(
        anio_defuncion,
        anio_declaracion,
        filas_base,
        filas_disponibles,
        filas_no_disponibles,
        defunciones
      ),
    by = c(
      "anio_defuncion",
      "anio_declaracion"
    )
  ) %>%
  mutate(
    estado_celda = case_when(
      
      is.na(filas_base) ~
        "Combinación ausente",
      
      filas_disponibles == 0 ~
        "Sin valores disponibles",
      
      defunciones == 0 ~
        "Representada solo con guiones",
      
      defunciones > 0 ~
        "Observada con positivos",
      
      TRUE ~
        "Revisar"
    )
  )


# ------------------------------------------------------------
# 14.5.3. RESUMIR LA COBERTURA POR AÑO DE OCURRENCIA
# ------------------------------------------------------------

resumen_cobertura_rezagos <- auditoria_cobertura_rezagos %>%
  group_by(anio_defuncion) %>%
  summarise(
    rezagos_esperados = n(),
    
    rezagos_observados =
      sum(!is.na(filas_base)),
    
    combinaciones_ausentes =
      sum(estado_celda == "Combinación ausente"),
    
    combinaciones_sin_disponibilidad =
      sum(estado_celda == "Sin valores disponibles"),
    
    combinaciones_solo_guiones =
      sum(estado_celda == "Representada solo con guiones"),
    
    combinaciones_con_positivos =
      sum(estado_celda == "Observada con positivos"),
    
    .groups = "drop"
  ) %>%
  mutate(
    cobertura_porcentual =
      100 * rezagos_observados / rezagos_esperados
  ) %>%
  arrange(anio_defuncion)


# ------------------------------------------------------------
# 14.5.4. IDENTIFICAR LAS COMBINACIONES AUSENTES
# ------------------------------------------------------------

detalle_combinaciones_ausentes <- auditoria_cobertura_rezagos %>%
  filter(
    estado_celda == "Combinación ausente"
  ) %>%
  arrange(
    anio_defuncion,
    retraso
  )


# ------------------------------------------------------------
# 14.5.5. RESUMEN GLOBAL DE LA COBERTURA
# ------------------------------------------------------------

resumen_global_cobertura_rezagos <- auditoria_cobertura_rezagos %>%
  count(
    estado_celda,
    name = "combinaciones"
  ) %>%
  mutate(
    total_combinaciones = sum(combinaciones),
    porcentaje =
      100 * combinaciones / total_combinaciones
  )


# ------------------------------------------------------------
# 14.5.6. VALIDACIONES
# ------------------------------------------------------------

comprobaciones_seccion_14_5 <- c(
  
  # La cuadrícula no contiene rezagos fuera de 0–9.
  rezagos_dentro_de_ventana =
    all(
      cuadricula_rezagos_esperados$retraso >= 0 &
        cuadricula_rezagos_esperados$retraso <= 9
    ),
  
  # Ninguna declaración supera el año de corte.
  declaraciones_dentro_del_corte =
    all(
      cuadricula_rezagos_esperados$anio_declaracion <= 2025
    ),
  
  # Una fila por combinación temporal esperada.
  cuadrícula_sin_duplicados =
    !anyDuplicated(
      cuadricula_rezagos_esperados %>%
        select(
          anio_defuncion,
          anio_declaracion
        )
    ),
  
  # La cuadrícula tiene 65 combinaciones esperadas.
  numero_combinaciones_esperadas =
    nrow(cuadricula_rezagos_esperados) == 65,
  
  # Las combinaciones observadas y ausentes reconstruyen
  # el total esperado.
  cobertura_reconstruida =
    sum(resumen_cobertura_rezagos$rezagos_observados) +
    sum(resumen_cobertura_rezagos$combinaciones_ausentes) ==
    nrow(cuadricula_rezagos_esperados),
  
  # El detalle de ausencias coincide con el resumen.
  ausencias_reconstruidas =
    nrow(detalle_combinaciones_ausentes) ==
    sum(resumen_cobertura_rezagos$combinaciones_ausentes),
  
  # Ninguna ausencia fue convertida en cero.
  ausencias_conservan_na =
    all(
      is.na(
        auditoria_cobertura_rezagos$filas_base[
          auditoria_cobertura_rezagos$estado_celda ==
            "Combinación ausente"
        ]
      )
    )
)


print(comprobaciones_seccion_14_5)

cat(
  "\nValidaciones superadas:",
  sum(comprobaciones_seccion_14_5),
  "de",
  length(comprobaciones_seccion_14_5),
  "\n"
)

stopifnot(all(comprobaciones_seccion_14_5))


# ------------------------------------------------------------
# 14.5.7. PRESENTAR RESULTADOS
# ------------------------------------------------------------

cat("\nCOBERTURA DE REZAGOS POR AÑO DE OCURRENCIA\n")

print(
  resumen_cobertura_rezagos,
  n = Inf,
  width = Inf
)


cat("\nCOMBINACIONES TEMPORALES AUSENTES\n")

print(
  detalle_combinaciones_ausentes,
  n = Inf
)


cat("\nDISTRIBUCIÓN GLOBAL DE ESTADOS DE LAS CELDAS\n")

print(
  resumen_global_cobertura_rezagos,
  n = Inf
)



# ============================================================
# CONSULTA COMPLEMENTARIA:
# PRESENCIA DE LAS CUATRO COMBINACIONES AUSENTES
# EN LOS DEMÁS UNIVERSOS REDATAM
# ============================================================


# ------------------------------------------------------------
# 1. RECUPERAR LOS REGISTROS DE LAS CUATRO COMBINACIONES
#    EN TODOS LOS UNIVERSOS
# ------------------------------------------------------------

detalle_ausencias_otros_universos <- datos_completos %>%
  semi_join(
    detalle_combinaciones_ausentes %>%
      select(
        anio_defuncion,
        anio_declaracion
      ),
    by = c(
      "anio_defuncion",
      "anio_declaracion"
    )
  ) %>%
  arrange(
    anio_defuncion,
    anio_declaracion,
    universo,
    sexo,
    provincia_id,
    edad_quinquenal
  )


# ------------------------------------------------------------
# 2. RESUMIR CADA COMBINACIÓN POR UNIVERSO
# ------------------------------------------------------------

resumen_ausencias_otros_universos <-
  detalle_ausencias_otros_universos %>%
  group_by(
    anio_defuncion,
    anio_declaracion,
    universo
  ) %>%
  summarise(
    filas_base = n(),
    
    filas_disponibles = sum(
      categoria_disponible,
      na.rm = TRUE
    ),
    
    filas_no_disponibles = sum(
      !categoria_disponible,
      na.rm = TRUE
    ),
    
    filas_positivas = sum(
      categoria_disponible &
        !is.na(defunciones) &
        defunciones > 0,
      na.rm = TRUE
    ),
    
    defunciones_disponibles = sum(
      defunciones[categoria_disponible],
      na.rm = TRUE
    ),
    
    .groups = "drop"
  ) %>%
  mutate(
    es_universo_referencia =
      universo == anio_declaracion
  ) %>%
  arrange(
    anio_defuncion,
    anio_declaracion,
    universo
  )


# ------------------------------------------------------------
# 3. RESUMIR LA PRESENCIA EN LOS 26 UNIVERSOS
# ------------------------------------------------------------

resumen_presencia_cuatro_combinaciones <-
  resumen_ausencias_otros_universos %>%
  group_by(
    anio_defuncion,
    anio_declaracion
  ) %>%
  summarise(
    universos_con_filas = n_distinct(universo),
    
    universos_con_positivos = sum(
      filas_positivas > 0
    ),
    
    universos_sin_positivos = sum(
      filas_positivas == 0
    ),
    
    universos_con_valores_no_disponibles = sum(
      filas_no_disponibles > 0
    ),
    
    universos_con_positivos_lista = paste(
      universo[filas_positivas > 0],
      collapse = ", "
    ),
    
    referencia_presente = any(
      es_universo_referencia
    ),
    
    .groups = "drop"
  ) %>%
  mutate(
    universos_sin_filas = 26 - universos_con_filas
  ) %>%
  arrange(
    anio_defuncion,
    anio_declaracion
  )


# ------------------------------------------------------------
# 4. PRESENTAR RESULTADOS
# ------------------------------------------------------------

cat(
  "\nRESUMEN DE PRESENCIA EN LOS UNIVERSOS REDATAM\n"
)

print(
  resumen_presencia_cuatro_combinaciones,
  n = Inf,
  width = Inf
)


cat(
  "\nDETALLE POR UNIVERSO\n"
)

print(
  resumen_ausencias_otros_universos,
  n = Inf,
  width = Inf
)


cat(
  "\nREGISTROS DESAGREGADOS POR SEXO, PROVINCIA Y EDAD\n"
)

print(
  detalle_ausencias_otros_universos,
  n = Inf,
  width = Inf
)


# ============================================================
# RESULTADO FINAL: PRESENCIA DE LAS CUATRO COMBINACIONES
# EN EL CONJUNTO COMPLETO DE UNIVERSOS REDATAM
# ============================================================

# Número real de universos incluidos en la base.
numero_universos_redatam <- n_distinct(
  datos_completos$universo
)

# Resumen de presencia, conservando las combinaciones
# aunque no tengan registros en ningún universo.

resumen_final_cuatro_combinaciones <-
  detalle_combinaciones_ausentes %>%
  distinct(
    anio_defuncion,
    anio_declaracion
  ) %>%
  left_join(
    resumen_ausencias_otros_universos %>%
      group_by(
        anio_defuncion,
        anio_declaracion
      ) %>%
      summarise(
        universos_con_filas = n_distinct(universo),
        universos_con_positivos = sum(filas_positivas > 0),
        universos_sin_positivos = sum(filas_positivas == 0),
        universos_con_valores_no_disponibles =
          sum(filas_no_disponibles > 0),
        universos_con_positivos_lista = paste(
          universo[filas_positivas > 0],
          collapse = ", "
        ),
        referencia_presente = any(es_universo_referencia),
        .groups = "drop"
      ),
    by = c(
      "anio_defuncion",
      "anio_declaracion"
    )
  ) %>%
  mutate(
    across(
      c(
        universos_con_filas,
        universos_con_positivos,
        universos_sin_positivos,
        universos_con_valores_no_disponibles
      ),
      ~ coalesce(.x, 0L)
    ),
    universos_con_positivos_lista = coalesce(
      universos_con_positivos_lista,
      "Ninguno"
    ),
    referencia_presente = coalesce(
      referencia_presente,
      FALSE
    ),
    universos_sin_filas =
      numero_universos_redatam - universos_con_filas,
    estado_presencia = if_else(
      universos_con_filas == 0,
      "Sin registros en ningún universo",
      "Con registros en uno o más universos"
    )
  ) %>%
  arrange(
    anio_defuncion,
    anio_declaracion
  )

print(
  resumen_final_cuatro_combinaciones,
  n = Inf,
  width = Inf
)



# ============================================================
# 15. ANÁLISIS DE OPORTUNIDAD DEL REGISTRO DE DEFUNCIONES
#     ADAPTACIÓN DEL EJEMPLO CEPAL - BRASIL
# ============================================================

# OBJETIVO:
#
# Examinar la evolución del porcentaje de defunciones
# registradas tardíamente según el año de ocurrencia.
#
# Se considera tardía una defunción cuyo año de declaración
# es posterior al año de ocurrencia.
#
# La unidad temporal de referencia es el año de ocurrencia.
#
# IMPORTANTE:
#
# 1. Se utiliza la matriz nacional construida en la sección 14.4.
# 2. El año de registro corresponde a anio_declaracion.
# 3. No se utiliza universo como sustituto del año de registro.
# 4. Las combinaciones ausentes permanecen como ausencias.
# 5. Los porcentajes con cobertura incompleta se conservan
#    como provisionales.
# 6. El año 2025 no recibe un porcentaje de 0 %, porque
#    solamente dispone del rezago cero.
# 7. Los resultados describen los registros observados.
#    No constituyen todavía una estimación corregida del
#    subregistro o de las defunciones tardías faltantes.


# ============================================================
# 15.1. INDICADOR NACIONAL DE REGISTRO TARDÍO
# ============================================================


# ------------------------------------------------------------
# 15.1.1. DEFINIR AÑO DE CORTE Y AÑOS DE OCURRENCIA
# ------------------------------------------------------------


anio_corte <- 2025

anios_ocurrencia <- 2015:2025


# ------------------------------------------------------------
# 15.1.2. CONSTRUIR LA CUADRÍCULA DE REZAGOS ESPERADOS
# ------------------------------------------------------------
#
# Para cada año de ocurrencia se consideran los rezagos
# desde cero hasta nueve años posteriores.
#
# El seguimiento se limita al año de corte 2025.
#
# Por ejemplo:
# - Ocurrencia 2015: rezagos 0 a 9.
# - Ocurrencia 2016: rezagos 0 a 9.
# - Ocurrencia 2025: solamente rezago 0.
#
# La ventana de diez años comprende el año oportuno
# y hasta nueve años de declaración posterior.


cuadricula_rezagos_esperados <-
  tidyr::expand_grid(
    anio_defuncion = anios_ocurrencia,
    retraso = 0:9
  ) %>%
  mutate(
    anio_declaracion =
      anio_defuncion + retraso
  ) %>%
  filter(
    anio_declaracion <= anio_corte
  ) %>%
  arrange(
    anio_defuncion,
    retraso
  )


# ------------------------------------------------------------
# 15.1.3. AUDITAR LA COBERTURA DE LOS REZAGOS
# ------------------------------------------------------------
#
# Se compara la cuadrícula esperada con la matriz observada.
#
# La presencia de una combinación se identifica mediante
# una variable explícita, independientemente del valor
# de defunciones.
#
# Esto permite distinguir:
#
# - Combinación ausente.
# - Combinación presente con cero.
# - Combinación presente con valores positivos.
#
# Las combinaciones ausentes no se convierten en ceros.


auditoria_cobertura_cepal <-
  cuadricula_rezagos_esperados %>%
  left_join(
    matriz_ocurrencia_declaracion %>%
      select(
        anio_defuncion,
        anio_declaracion,
        retraso,
        defunciones
      ) %>%
      mutate(
        combinacion_observada = TRUE
      ),
    by = c(
      "anio_defuncion",
      "anio_declaracion",
      "retraso"
    )
  ) %>%
  mutate(
    combinacion_observada =
      coalesce(combinacion_observada, FALSE),
    
    estado_combinacion = case_when(
      
      !combinacion_observada ~
        "Combinación ausente",
      
      is.na(defunciones) ~
        "Observada sin valor disponible",
      
      defunciones == 0 ~
        "Representada solo con guiones",
      
      defunciones > 0 ~
        "Observada con positivos",
      
      TRUE ~
        "Revisar"
    )
  )


# ------------------------------------------------------------
# 15.1.4. RESUMIR LA COBERTURA POR AÑO DE OCURRENCIA
# ------------------------------------------------------------


resumen_cobertura_cepal <-
  auditoria_cobertura_cepal %>%
  group_by(
    anio_defuncion
  ) %>%
  summarise(
    
    rezagos_esperados = n(),
    
    rezagos_observados = sum(
      combinacion_observada
    ),
    
    rezagos_ausentes = sum(
      !combinacion_observada
    ),
    
    rezagos_sin_valor = sum(
      combinacion_observada &
        is.na(defunciones)
    ),
    
    cobertura_completa = all(
      combinacion_observada
    ),
    
    porcentaje_cobertura =
      100 * rezagos_observados / rezagos_esperados,
    
    .groups = "drop"
  )


# ------------------------------------------------------------
# 15.1.5. CALCULAR DEFUNCIONES OPORTUNAS Y TARDÍAS
#         OBSERVADAS
# ------------------------------------------------------------
#
# Los conteos se calculan exclusivamente a partir de las
# combinaciones observadas.
#
# Las combinaciones ausentes no aportan cero al numerador
# ni al denominador.
#
# El porcentaje se calcula sobre el total observado:
#
#       Defunciones tardías observadas
#       ---------------------------------- x 100
#       Total de defunciones observadas
#
# Si existen rezagos tardíos observados, el porcentaje
# se conserva incluso cuando la cobertura es incompleta.
#
# En estos casos se interpreta como provisional.


resumen_defunciones_cepal <-
  auditoria_cobertura_cepal %>%
  group_by(
    anio_defuncion
  ) %>%
  summarise(
    
    defunciones_oportunas = sum(
      defunciones[
        combinacion_observada &
          retraso == 0
      ],
      na.rm = TRUE
    ),
    
    defunciones_tardias = sum(
      defunciones[
        combinacion_observada &
          retraso > 0
      ],
      na.rm = TRUE
    ),
    
    total_defunciones_observadas = sum(
      defunciones[
        combinacion_observada
      ],
      na.rm = TRUE
    ),
    
    rezagos_tardios_observados = sum(
      combinacion_observada &
        retraso > 0
    ),
    
    .groups = "drop"
  )


# ------------------------------------------------------------
# 15.1.6. INTEGRAR RESULTADOS Y CLASIFICAR LOS AÑOS
# ------------------------------------------------------------
#
# Se mantienen separadas tres dimensiones:
#
# 1. Cobertura de la matriz.
# 2. Seguimiento temporal.
# 3. Interpretación del porcentaje.
#
# El criterio de cinco años de seguimiento se adopta
# operativamente del ejemplo CEPAL-Brasil.
#
# No garantiza por sí mismo que el registro esté
# definitivamente cerrado.


tabla_oportunidad_cepal <-
  resumen_cobertura_cepal %>%
  left_join(
    resumen_defunciones_cepal,
    by = "anio_defuncion"
  ) %>%
  mutate(
    
    # --------------------------------------------------------
    # Porcentaje observado de defunciones tardías
    # --------------------------------------------------------
    
    pct_tardias_observado = case_when(
      
      rezagos_tardios_observados > 0 &
        total_defunciones_observadas > 0 ~
        
        100 * defunciones_tardias /
        total_defunciones_observadas,
      
      TRUE ~ NA_real_
    ),
    
    # --------------------------------------------------------
    # Seguimiento temporal
    # --------------------------------------------------------
    
    anios_seguimiento =
      anio_corte - anio_defuncion,
    
    estado_seguimiento = case_when(
      
      anios_seguimiento == 0 ~
        "Sin seguimiento posterior",
      
      anios_seguimiento < 5 ~
        "Seguimiento incompleto",
      
      anios_seguimiento >= 5 ~
        "Consolidado",
      
      TRUE ~ "Revisar"
    ),
    
    # --------------------------------------------------------
    # Cobertura de la matriz
    # --------------------------------------------------------
    
    estado_cobertura = case_when(
      
      cobertura_completa ~
        "Cobertura completa",
      
      TRUE ~
        "Cobertura incompleta"
    ),
    
    # --------------------------------------------------------
    # Interpretación del porcentaje
    # --------------------------------------------------------
    
    interpretacion_porcentaje = case_when(
      
      is.na(pct_tardias_observado) &
        estado_seguimiento ==
        "Sin seguimiento posterior" ~
        
        "No calculable: sin seguimiento posterior",
      
      is.na(pct_tardias_observado) ~
        
        "No calculable: sin rezagos tardíos observados",
      
      !cobertura_completa ~
        
        "Provisional: cobertura incompleta",
      
      TRUE ~
        
        "Porcentaje observado: cobertura completa"
    ),
    
    # --------------------------------------------------------
    # Clasificación general para presentación
    # --------------------------------------------------------
    
    estado_analisis = case_when(
      
      !cobertura_completa ~
        
        "Cobertura incompleta",
      
      estado_seguimiento ==
        "Sin seguimiento posterior" ~
        
        "Sin seguimiento posterior",
      
      estado_seguimiento ==
        "Seguimiento incompleto" ~
        
        "Seguimiento incompleto",
      
      TRUE ~
        
        "Consolidado"
    )
  ) %>%
  arrange(
    anio_defuncion
  ) %>%
  select(
    anio_defuncion,
    rezagos_esperados,
    rezagos_observados,
    rezagos_ausentes,
    rezagos_sin_valor,
    cobertura_completa,
    porcentaje_cobertura,
    defunciones_oportunas,
    defunciones_tardias,
    total_defunciones_observadas,
    rezagos_tardios_observados,
    pct_tardias_observado,
    anios_seguimiento,
    estado_cobertura,
    estado_seguimiento,
    interpretacion_porcentaje,
    estado_analisis
  )


# ------------------------------------------------------------
# 15.1.7. PRESENTAR LA TABLA DE RESULTADOS
# ------------------------------------------------------------


cat(
  "\nEVOLUCIÓN NACIONAL DEL REGISTRO TARDÍO\n"
)

print(
  tabla_oportunidad_cepal,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------
# 15.1.8. IDENTIFICAR LAS COMBINACIONES AUSENTES
# ------------------------------------------------------------


detalle_ausencias_cepal <-
  auditoria_cobertura_cepal %>%
  filter(
    !combinacion_observada
  ) %>%
  arrange(
    anio_defuncion,
    retraso
  ) %>%
  select(
    anio_defuncion,
    retraso,
    anio_declaracion,
    estado_combinacion
  )


cat(
  "\nCOMBINACIONES TEMPORALES AUSENTES\n"
)

print(
  detalle_ausencias_cepal,
  n = Inf
)


# ------------------------------------------------------------
# 15.1.9. VALIDACIONES DEL INDICADOR
# ------------------------------------------------------------


comprobaciones_cepal <- c(
  
  # Se conservan los once años de ocurrencia.
  numero_anios_correcto =
    nrow(tabla_oportunidad_cepal) ==
    length(anios_ocurrencia),
  
  # La serie cubre exactamente 2015-2025.
  rango_anios_correcto =
    identical(
      tabla_oportunidad_cepal$anio_defuncion,
      anios_ocurrencia
    ),
  
  # No existen duplicados en la matriz original
  # para las combinaciones año de ocurrencia-declaración.
  matriz_sin_duplicados =
    nrow(matriz_ocurrencia_declaracion) ==
    nrow(
      distinct(
        matriz_ocurrencia_declaracion,
        anio_defuncion,
        anio_declaracion
      )
    ),
  
  # No hay duplicados en la cuadrícula esperada.
  cuadricula_sin_duplicados =
    nrow(cuadricula_rezagos_esperados) ==
    nrow(
      distinct(
        cuadricula_rezagos_esperados,
        anio_defuncion,
        anio_declaracion,
        retraso
      )
    ),
  
  # La cuadrícula contiene 65 combinaciones.
  numero_combinaciones_esperadas_correcto =
    nrow(cuadricula_rezagos_esperados) == 65,
  
  # Todas las declaraciones están dentro del año de corte.
  limite_temporal_correcto =
    all(
      cuadricula_rezagos_esperados$anio_declaracion <=
        anio_corte
    ),
  
  # Las combinaciones observadas y ausentes reconstruyen
  # la cuadrícula esperada.
  cobertura_reconstruye_cuadricula =
    sum(
      tabla_oportunidad_cepal$rezagos_observados +
        tabla_oportunidad_cepal$rezagos_ausentes
    ) ==
    nrow(cuadricula_rezagos_esperados),
  
  # Las ausencias se mantienen identificadas.
  ausencias_conservadas =
    sum(
      tabla_oportunidad_cepal$rezagos_ausentes
    ) ==
    nrow(detalle_ausencias_cepal),
  
  # Las ausencias no se convierten en ceros.
  ausencias_no_imputadas =
    all(
      is.na(
        auditoria_cobertura_cepal$defunciones[
          !auditoria_cobertura_cepal$combinacion_observada
        ]
      )
    ),
  
  # No existen combinaciones observadas sin valor disponible.

  observaciones_con_valores_disponibles =
    all(
      resumen_cobertura_cepal$rezagos_sin_valor == 0
    ),
  
  # Los porcentajes calculados están entre 0 y 100.
  porcentajes_en_rango =
    all(
      tabla_oportunidad_cepal$pct_tardias_observado[
        !is.na(tabla_oportunidad_cepal$pct_tardias_observado)
      ] >= 0 &
        tabla_oportunidad_cepal$pct_tardias_observado[
          !is.na(tabla_oportunidad_cepal$pct_tardias_observado)
        ] <= 100
    ),
  
  # Los porcentajes requieren rezagos tardíos observados.
  porcentajes_con_rezagos_tardios =
    all(
      is.na(
        tabla_oportunidad_cepal$pct_tardias_observado[
          tabla_oportunidad_cepal$rezagos_tardios_observados == 0
        ]
      )
    ),
  
  # El año 2025 se identifica sin seguimiento posterior.
  anio_2025_sin_seguimiento =
    tabla_oportunidad_cepal$estado_seguimiento[
      tabla_oportunidad_cepal$anio_defuncion == 2025
    ] == "Sin seguimiento posterior",
  
  # El porcentaje de 2025 no se interpreta como 0 %.
  anio_2025_porcentaje_no_calculado =
    is.na(
      tabla_oportunidad_cepal$pct_tardias_observado[
        tabla_oportunidad_cepal$anio_defuncion == 2025
      ]
    ),
  
  # Los años con cobertura incompleta permanecen en la tabla.
  anios_con_cobertura_incompleta_conservados =
    all(
      c(2015, 2016, 2018) %in%
        tabla_oportunidad_cepal$anio_defuncion[
          !tabla_oportunidad_cepal$cobertura_completa
        ]
    ),
  
  # Los tres años incompletos conservan porcentajes provisionales.
  porcentajes_provisionales_conservados =
    all(
      !is.na(
        tabla_oportunidad_cepal$pct_tardias_observado[
          tabla_oportunidad_cepal$anio_defuncion %in%
            c(2015, 2016, 2018)
        ]
      )
    ),
  
  # Las cuatro ausencias conocidas permanecen identificadas.
  numero_ausencias_correcto =
    nrow(detalle_ausencias_cepal) == 4,
  
  # El porcentaje reproduce la operación definida.
  calculo_porcentajes_correcto =
    all(
      abs(
        tabla_oportunidad_cepal$pct_tardias_observado[
          !is.na(tabla_oportunidad_cepal$pct_tardias_observado)
        ] -
          100 *
          tabla_oportunidad_cepal$defunciones_tardias[
            !is.na(tabla_oportunidad_cepal$pct_tardias_observado)
          ] /
          tabla_oportunidad_cepal$total_defunciones_observadas[
            !is.na(tabla_oportunidad_cepal$pct_tardias_observado)
          ]
      ) < 1e-10
    )
)


print(
  comprobaciones_cepal
)


# ============================================================
# 15.1.10. DETENER EL SCRIPT SI ALGUNA VALIDACIÓN FALLA
# ============================================================

print(
  comprobaciones_cepal
)

stopifnot(
  all(comprobaciones_cepal)
)

# ------------------------------------------------------------
# 15.1.11. PREPARAR LOS DATOS PARA LOS GRÁFICOS
# ------------------------------------------------------------
#
# Se conservan los once años en el eje horizontal.
#
# Los porcentajes provisionales se muestran diferenciados.
#
# El año 2025 permanece en el eje, pero sin punto,
# porque no tiene seguimiento posterior.
#
# Los segmentos de línea se interrumpen si falta algún
# año con porcentaje calculable.


datos_grafico_oportunidad <-
  tabla_oportunidad_cepal %>%
  mutate(
    
    estado_grafico = factor(
      estado_analisis,
      levels = c(
        "Consolidado",
        "Seguimiento incompleto",
        "Cobertura incompleta",
        "Sin seguimiento posterior"
      )
    )
  ) %>%
  arrange(
    anio_defuncion
  )


datos_linea_oportunidad <-
  datos_grafico_oportunidad %>%
  filter(
    !is.na(pct_tardias_observado)
  ) %>%
  arrange(
    anio_defuncion
  ) %>%
  mutate(
    
    grupo_tramo = cumsum(
      c(
        TRUE,
        diff(anio_defuncion) > 1
      )
    )
  )


# ------------------------------------------------------------
# 15.1.12. GRÁFICO NACIONAL EN ESPAÑOL
# ------------------------------------------------------------


grafico_oportunidad_cepal <-
  ggplot2::ggplot() +
  
  ggplot2::geom_line(
    data = datos_linea_oportunidad,
    ggplot2::aes(
      x = anio_defuncion,
      y = pct_tardias_observado,
      group = grupo_tramo
    ),
    color = "grey40",
    linewidth = 0.8
  ) +
  
  ggplot2::geom_point(
    data = datos_grafico_oportunidad %>%
      filter(
        !is.na(pct_tardias_observado)
      ),
    ggplot2::aes(
      x = anio_defuncion,
      y = pct_tardias_observado,
      color = estado_grafico
    ),
    size = 3
  ) +
  
  ggplot2::scale_x_continuous(
    breaks = anios_ocurrencia,
    limits = range(anios_ocurrencia)
  ) +
  
  ggplot2::scale_y_continuous(
    limits = c(0, NA),
    expand = ggplot2::expansion(
      mult = c(0, 0.08)
    )
  ) +
  
  ggplot2::labs(
    
    title =
      "Oportunidad del registro de defunciones en Costa Rica",
    
    subtitle =
      paste(
        "Porcentaje de defunciones tardías observadas",
        "según año de ocurrencia"
      ),
    
    x = "Año de ocurrencia",
    
    y = "Defunciones tardías observadas (%)",
    
    color = "Estado del análisis",
    
    caption =
      paste(
        "Nota: los porcentajes con cobertura incompleta",
        "son provisionales y se calculan sobre las",
        "combinaciones observadas.",
        "El año 2025 no presenta porcentaje por ausencia",
        "de seguimiento posterior.",
        "Las combinaciones ausentes no se imputaron como cero."
      )
  ) +
  
  ggplot2::theme_minimal(
    base_size = 12
  ) +
  
  ggplot2::theme(
    
    plot.title = ggplot2::element_text(
      face = "bold"
    ),
    
    axis.text.x = ggplot2::element_text(
      angle = 45,
      hjust = 1
    ),
    
    legend.position = "bottom",
    
    plot.caption = ggplot2::element_text(
      hjust = 0,
      size = 9
    )
  )


print(
  grafico_oportunidad_cepal
)


# ------------------------------------------------------------
# 15.1.13. GRÁFICO NACIONAL EN INGLÉS
# ------------------------------------------------------------


datos_grafico_oportunidad_en <-
  datos_grafico_oportunidad %>%
  mutate(
    
    estado_grafico_en = case_when(
      
      estado_analisis == "Consolidado" ~
        "Consolidated",
      
      estado_analisis == "Seguimiento incompleto" ~
        "Incomplete follow-up",
      
      estado_analisis == "Cobertura incompleta" ~
        "Incomplete coverage",
      
      estado_analisis == "Sin seguimiento posterior" ~
        "No subsequent follow-up",
      
      TRUE ~ "Review"
    ),
    
    estado_grafico_en = factor(
      estado_grafico_en,
      levels = c(
        "Consolidated",
        "Incomplete follow-up",
        "Incomplete coverage",
        "No subsequent follow-up"
      )
    )
  )


datos_linea_oportunidad_en <-
  datos_grafico_oportunidad_en %>%
  filter(
    !is.na(pct_tardias_observado)
  ) %>%
  arrange(
    anio_defuncion
  ) %>%
  mutate(
    
    grupo_tramo = cumsum(
      c(
        TRUE,
        diff(anio_defuncion) > 1
      )
    )
  )


grafico_oportunidad_cepal_en <-
  ggplot2::ggplot() +
  
  ggplot2::geom_line(
    data = datos_linea_oportunidad_en,
    ggplot2::aes(
      x = anio_defuncion,
      y = pct_tardias_observado,
      group = grupo_tramo
    ),
    color = "grey40",
    linewidth = 0.8
  ) +
  
  ggplot2::geom_point(
    data = datos_grafico_oportunidad_en %>%
      filter(
        !is.na(pct_tardias_observado)
      ),
    ggplot2::aes(
      x = anio_defuncion,
      y = pct_tardias_observado,
      color = estado_grafico_en
    ),
    size = 3
  ) +
  
  ggplot2::scale_x_continuous(
    breaks = anios_ocurrencia,
    limits = range(anios_ocurrencia)
  ) +
  
  ggplot2::scale_y_continuous(
    limits = c(0, NA),
    expand = ggplot2::expansion(
      mult = c(0, 0.08)
    )
  ) +
  
  ggplot2::labs(
    
    title =
      "Timeliness of Death Registration in Costa Rica",
    
    subtitle =
      "Observed percentage of late-registered deaths by year of occurrence",
    
    x = "Year of occurrence",
    
    y = "Observed late-registered deaths (%)",
    
    color = "Analysis status",
    
    caption =
      paste(
        "Note: percentages with incomplete coverage are",
        "provisional and calculated from observed combinations.",
        "No percentage is shown for 2025 because there is",
        "no subsequent follow-up.",
        "Missing combinations were not imputed as zero."
      )
  ) +
  
  ggplot2::theme_minimal(
    base_size = 12
  ) +
  
  ggplot2::theme(
    
    plot.title = ggplot2::element_text(
      face = "bold"
    ),
    
    axis.text.x = ggplot2::element_text(
      angle = 45,
      hjust = 1
    ),
    
    legend.position = "bottom",
    
    plot.caption = ggplot2::element_text(
      hjust = 0,
      size = 9
    )
  )


print(
  grafico_oportunidad_cepal_en
)


# ------------------------------------------------------------
# 15.1.14. COMBINAR LOS GRÁFICOS EN ESPAÑOL E INGLÉS
# ------------------------------------------------------------
#
# Requiere el paquete patchwork.
#
# Se utiliza wrap_plots() para combinar ambos gráficos
# sin depender del operador /.
#
# Instalar una sola vez si todavía no está disponible:
# install.packages("patchwork")


grafico_oportunidad_bilingue <-
  patchwork::wrap_plots(
    grafico_oportunidad_cepal,
    grafico_oportunidad_cepal_en,
    ncol = 1
  ) +
  patchwork::plot_annotation(
    title =
      "Oportunidad del registro de defunciones en Costa Rica / Timeliness of Death Registration in Costa Rica",
    tag_levels = "A"
  )


print(
  grafico_oportunidad_bilingue
)

# ------------------------------------------------------------
# 15.1.15. GUARDAR LOS GRÁFICOS
# ------------------------------------------------------------


ggplot2::ggsave(
  filename =
    "oportunidad_registro_defunciones_Costa_Rica.png",
  plot = grafico_oportunidad_cepal,
  width = 12,
  height = 7,
  units = "in",
  dpi = 300
)


ggplot2::ggsave(
  filename =
    "oportunidad_registro_defunciones_Costa_Rica_English.png",
  plot = grafico_oportunidad_cepal_en,
  width = 12,
  height = 7,
  units = "in",
  dpi = 300
)


ggplot2::ggsave(
  filename =
    "oportunidad_registro_defunciones_bilingue.png",
  plot = grafico_oportunidad_bilingue,
  width = 12,
  height = 12,
  units = "in",
  dpi = 300
)


# ------------------------------------------------------------
# 15.1.16. MENSAJE DE CIERRE
# ------------------------------------------------------------


cat(
  "\n",
  "SECCIÓN 15.1 COMPLETADA.\n",
  "Se conservaron todos los años de ocurrencia 2015-2025.\n",
  "La cobertura y el seguimiento temporal se clasificaron\n",
  "por separado.\n",
  "Los porcentajes con cobertura incompleta se conservaron\n",
  "como provisionales.\n",
  "El año 2025 se mantuvo sin porcentaje por ausencia de\n",
  "seguimiento posterior.\n",
  "Las combinaciones ausentes no se imputaron como ceros.\n",
  "Se generaron los gráficos en español, inglés y bilingüe.\n",
  sep = ""
)

# ============================================================
# FIN DE LA SECCIÓN 15.1
# ============================================================


# ============================================================
# 15.2. INDICADOR DE REGISTRO TARDÍO POR SEXO Y PROVINCIA
#      ADAPTACIÓN DEL EJEMPLO CEPAL - BRASIL
# ============================================================
#
# OBJETIVO:
#
# Calcular el porcentaje observado de defunciones tardías
# por año de ocurrencia, sexo y provincia.
#
# CRITERIOS METODOLÓGICOS:
#
# 1. Utilizar el universo REDATAM correspondiente al año
#    de declaración: universo == anio_declaracion.
#
# 2. No sumar universos REDATAM consecutivos.
#
# 3. Agregar las categorías de edad para obtener los
#    conteos por año, sexo y provincia.
#
# 4. Distinguir combinaciones observadas y ausentes.
#
# 5. No convertir combinaciones ausentes en ceros.
#
# 6. Calcular porcentajes provisionales cuando la cobertura
#    sea incompleta, siempre que exista el rezago cero.
#
# 7. Mantener 2025 sin porcentaje por falta de seguimiento
#    posterior.
#
# 8. Los indicadores son descriptivos y no representan
#    estimaciones corregidas de mortalidad.


# ------------------------------------------------------------
# 15.2.1. DEFINIR PARÁMETROS
# ------------------------------------------------------------

anio_corte <- 2025

anios_ocurrencia <- 2015:2025


# Identificar los estratos disponibles.

estratos_sexo <-
  datos_completos %>%
  distinct(sexo) %>%
  arrange(sexo)

estratos_provincia <-
  datos_completos %>%
  distinct(
    provincia_id,
    provincia
  ) %>%
  arrange(provincia_id)


# ------------------------------------------------------------
# 15.2.2. CONSTRUIR LA MATRIZ DE REFERENCIA
# ------------------------------------------------------------
#
# Se selecciona el universo correspondiente al año
# de declaración.
#
# Se agregan las edades para obtener los conteos
# por ocurrencia, declaración, sexo y provincia.
#
# Se conserva únicamente la información disponible.

matriz_referencia_sexo_provincia <-
  datos_completos %>%
  filter(
    universo == anio_declaracion,
    anio_declaracion >= min(anios_ocurrencia),
    anio_declaracion <= anio_corte,
    anio_defuncion %in% anios_ocurrencia,
    categoria_disponible
  ) %>%
  group_by(
    anio_defuncion,
    anio_declaracion,
    sexo,
    provincia_id,
    provincia
  ) %>%
  summarise(
    defunciones = sum(
      defunciones,
      na.rm = TRUE
    ),
    filas_edad = n(),
    .groups = "drop"
  ) %>%
  mutate(
    retraso =
      anio_declaracion - anio_defuncion
  ) %>%
  arrange(
    anio_defuncion,
    sexo,
    provincia_id,
    retraso
  )


# ------------------------------------------------------------
# 15.2.3. CONSTRUIR LA CUADRÍCULA DE REZAGOS ESPERADOS
# ------------------------------------------------------------
#
# Para cada año de ocurrencia se incluyen los rezagos
# desde 0 hasta 9, limitados al año de corte.
#
# Se combinan los 11 años, los 2 sexos y las 7 provincias.
#
# Total esperado: 910 combinaciones.

cuadricula_rezagos_sexo_provincia <-
  tidyr::expand_grid(
    anio_defuncion = anios_ocurrencia,
    retraso = 0:9,
    sexo = estratos_sexo$sexo,
    provincia_id = estratos_provincia$provincia_id
  ) %>%
  left_join(
    estratos_provincia,
    by = "provincia_id"
  ) %>%
  mutate(
    anio_declaracion =
      anio_defuncion + retraso
  ) %>%
  filter(
    anio_declaracion <= anio_corte
  ) %>%
  arrange(
    anio_defuncion,
    sexo,
    provincia_id,
    retraso
  )


# ------------------------------------------------------------
# 15.2.4. AUDITAR LA COBERTURA DE LOS REZAGOS
# ------------------------------------------------------------
#
# La variable combinacion_observada permite distinguir
# las ausencias de los ceros observados.

auditoria_cobertura_sexo_provincia <-
  cuadricula_rezagos_sexo_provincia %>%
  left_join(
    matriz_referencia_sexo_provincia %>%
      select(
        anio_defuncion,
        anio_declaracion,
        retraso,
        sexo,
        provincia_id,
        defunciones
      ) %>%
      mutate(
        combinacion_observada = TRUE
      ),
    by = c(
      "anio_defuncion",
      "anio_declaracion",
      "retraso",
      "sexo",
      "provincia_id"
    )
  ) %>%
  mutate(
    combinacion_observada =
      coalesce(
        combinacion_observada,
        FALSE
      ),
    
    estado_combinacion = case_when(
      
      !combinacion_observada ~
        "Combinación ausente",
      
      is.na(defunciones) ~
        "Observada sin valor disponible",
      
      defunciones == 0 ~
        "Representada solo con guiones",
      
      defunciones > 0 ~
        "Observada con positivos",
      
      TRUE ~ "Revisar"
    )
  )

# ------------------------------------------------------------
# 15.2.5. RESUMIR LA COBERTURA POR ESTRATO
# ------------------------------------------------------------

resumen_cobertura_sexo_provincia <-
  auditoria_cobertura_sexo_provincia %>%
  group_by(
    anio_defuncion,
    sexo,
    provincia_id,
    provincia
  ) %>%
  summarise(
    
    rezagos_esperados = n(),
    
    rezagos_observados = sum(
      combinacion_observada
    ),
    
    rezagos_ausentes = sum(
      !combinacion_observada
    ),
    
    rezagos_sin_valor = sum(
      combinacion_observada &
        is.na(defunciones)
    ),
    
    rezago_cero_observado = any(
      combinacion_observada &
        retraso == 0
    ),
    
    cobertura_completa = all(
      combinacion_observada
    ),
    
    porcentaje_cobertura =
      100 * rezagos_observados /
      rezagos_esperados,
    
    .groups = "drop"
  )


# ------------------------------------------------------------
# 15.2.6. CALCULAR DEFUNCIONES OPORTUNAS Y TARDÍAS
# ------------------------------------------------------------
#
# El rezago cero representa las defunciones oportunas.
# Los rezagos mayores que cero representan las tardías.
#
# Solo se suman los conteos de combinaciones observadas.
# Las combinaciones ausentes no se consideran ceros.

resumen_defunciones_sexo_provincia <-
  auditoria_cobertura_sexo_provincia %>%
  group_by(
    anio_defuncion,
    sexo,
    provincia_id,
    provincia
  ) %>%
  summarise(
    
    defunciones_oportunas = if (
      any(
        combinacion_observada &
        retraso == 0
      )
    ) {
      sum(
        defunciones[
          combinacion_observada &
            retraso == 0
        ],
        na.rm = TRUE
      )
    } else {
      NA_real_
    },
    
    defunciones_tardias = sum(
      defunciones[
        combinacion_observada &
          retraso > 0
      ],
      na.rm = TRUE
    ),
    
    total_defunciones_observadas = sum(
      defunciones[
        combinacion_observada
      ],
      na.rm = TRUE
    ),
    
    rezagos_tardios_observados = sum(
      combinacion_observada &
        retraso > 0
    ),
    
    .groups = "drop"
  )



# ------------------------------------------------------------
# 15.2.7. CALCULAR EL PORCENTAJE DE REGISTRO TARDÍO
# ------------------------------------------------------------
#
# Fórmula:
#
# Defunciones tardías observadas /
# Total de defunciones observadas x 100
#
# El porcentaje se calcula cuando:
# 1. Existe seguimiento posterior.
# 2. El rezago cero está observado.
# 3. Existe al menos un rezago tardío observado.
# 4. El total de defunciones observadas es mayor que cero.
#
# Si no hay rezagos tardíos observados, no se asigna 0 %,
# porque no se puede distinguir entre ausencia de registros
# tardíos y ausencia de información sobre esos rezagos.
#
# El año 2025 permanece sin porcentaje por falta de
# seguimiento posterior.

tabla_oportunidad_sexo_provincia <-
  resumen_cobertura_sexo_provincia %>%
  left_join(
    resumen_defunciones_sexo_provincia,
    by = c(
      "anio_defuncion",
      "sexo",
      "provincia_id",
      "provincia"
    )
  ) %>%
  mutate(
    
    anios_seguimiento =
      anio_corte - anio_defuncion,
    
    pct_tardias_observado = case_when(
      anios_seguimiento == 0 ~ NA_real_,
      !rezago_cero_observado ~ NA_real_,
      rezagos_tardios_observados == 0 ~ NA_real_,
      total_defunciones_observadas <= 0 ~ NA_real_,
      TRUE ~
        100 * defunciones_tardias /
        total_defunciones_observadas
    ),
    
    interpretacion_porcentaje = case_when(
      anios_seguimiento == 0 ~
        "No calculable: sin seguimiento posterior",
      
      !rezago_cero_observado ~
        "No calculable: falta rezago cero",
      
      rezagos_tardios_observados == 0 ~
        "No calculable: sin rezagos tardíos observados",
      
      total_defunciones_observadas <= 0 ~
        "No calculable: total observado igual a cero",
      
      !cobertura_completa ~
        "Provisional: cobertura incompleta",
      
      TRUE ~
        "Porcentaje observado: cobertura completa"
    ),
    
    estado_cobertura = case_when(
      !cobertura_completa ~
        "Cobertura incompleta",
      
      TRUE ~
        "Cobertura completa"
    ),
    
    estado_seguimiento = case_when(
      anios_seguimiento == 0 ~
        "Sin seguimiento posterior",
      
      anios_seguimiento < 5 ~
        "Seguimiento incompleto",
      
      anios_seguimiento >= 5 ~
        "Consolidado",
      
      TRUE ~ "Revisar"
    ),
    
    estado_analisis = case_when(
      anios_seguimiento == 0 ~
        "Sin seguimiento posterior",
      
      !rezago_cero_observado ~
        "Falta rezago cero",
      
      !cobertura_completa ~
        "Cobertura incompleta",
      
      anios_seguimiento < 5 ~
        "Seguimiento incompleto",
      
      TRUE ~ "Consolidado"
    )
  ) %>%
  arrange(
    anio_defuncion,
    provincia_id,
    sexo
  ) %>%
  select(
    anio_defuncion,
    sexo,
    provincia_id,
    provincia,
    rezagos_esperados,
    rezagos_observados,
    rezagos_ausentes,
    rezagos_sin_valor,
    rezago_cero_observado,
    cobertura_completa,
    porcentaje_cobertura,
    defunciones_oportunas,
    defunciones_tardias,
    total_defunciones_observadas,
    rezagos_tardios_observados,
    pct_tardias_observado,
    anios_seguimiento,
    estado_cobertura,
    estado_seguimiento,
    interpretacion_porcentaje,
    estado_analisis
  )


# ------------------------------------------------------------
# 15.2.8. PRESENTAR RESULTADOS POR SEXO Y PROVINCIA
# ------------------------------------------------------------

cat(
  "\nINDICADOR DE REGISTRO TARDÍO POR SEXO Y PROVINCIA\n"
)

print(
  tabla_oportunidad_sexo_provincia,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------
# 15.2.9. IDENTIFICAR COMBINACIONES AUSENTES
# ------------------------------------------------------------

detalle_ausencias_sexo_provincia <-
  auditoria_cobertura_sexo_provincia %>%
  filter(
    !combinacion_observada
  ) %>%
  arrange(
    anio_defuncion,
    provincia_id,
    sexo,
    retraso
  ) %>%
  select(
    anio_defuncion,
    sexo,
    provincia_id,
    provincia,
    retraso,
    anio_declaracion,
    estado_combinacion
  )

cat(
  "\nCOMBINACIONES AUSENTES POR SEXO Y PROVINCIA\n"
)

print(
  detalle_ausencias_sexo_provincia,
  n = Inf
)


# ------------------------------------------------------------
# 15.2.10. RESUMEN DE COBERTURA POR SEXO
# ------------------------------------------------------------

resumen_oportunidad_por_sexo <-
  tabla_oportunidad_sexo_provincia %>%
  group_by(
    anio_defuncion,
    sexo
  ) %>%
  summarise(
    
    provincias = n(),
    
    provincias_con_porcentaje = sum(
      !is.na(pct_tardias_observado)
    ),
    
    provincias_con_cobertura_incompleta = sum(
      !cobertura_completa
    ),
    
    .groups = "drop"
  ) %>%
  arrange(
    anio_defuncion,
    sexo
  )

cat(
  "\nRESUMEN DE COBERTURA POR SEXO\n"
)

print(
  resumen_oportunidad_por_sexo,
  n = Inf
)


# ------------------------------------------------------------
# 15.2.11. RESUMEN DE COBERTURA POR PROVINCIA
# ------------------------------------------------------------

resumen_oportunidad_por_provincia <-
  tabla_oportunidad_sexo_provincia %>%
  group_by(
    anio_defuncion,
    provincia_id,
    provincia
  ) %>%
  summarise(
    
    sexos = n(),
    
    sexos_con_porcentaje = sum(
      !is.na(pct_tardias_observado)
    ),
    
    sexos_con_cobertura_incompleta = sum(
      !cobertura_completa
    ),
    
    .groups = "drop"
  ) %>%
  arrange(
    anio_defuncion,
    provincia_id
  )

cat(
  "\nRESUMEN DE COBERTURA POR PROVINCIA\n"
)

print(
  resumen_oportunidad_por_provincia,
  n = Inf
)


# ------------------------------------------------------------
# 15.2.12. VALIDACIONES
# ------------------------------------------------------------

comprobaciones_sexo_provincia <- c(
  
  numero_anios_correcto =
    length(anios_ocurrencia) == 11,
  
  numero_sexos_correcto =
    nrow(estratos_sexo) == 2,
  
  numero_provincias_correcto =
    nrow(estratos_provincia) == 7,
  
  numero_estratos_correcto =
    nrow(tabla_oportunidad_sexo_provincia) == 154,
  
  matriz_sin_duplicados =
    nrow(matriz_referencia_sexo_provincia) ==
    nrow(
      distinct(
        matriz_referencia_sexo_provincia,
        anio_defuncion,
        anio_declaracion,
        sexo,
        provincia_id
      )
    ),
  
  cuadricula_sin_duplicados =
    nrow(cuadricula_rezagos_sexo_provincia) ==
    nrow(
      distinct(
        cuadricula_rezagos_sexo_provincia,
        anio_defuncion,
        anio_declaracion,
        retraso,
        sexo,
        provincia_id
      )
    ),
  
  numero_combinaciones_esperadas_correcto =
    nrow(cuadricula_rezagos_sexo_provincia) == 910,
  
  cobertura_reconstruye_cuadricula =
    sum(
      tabla_oportunidad_sexo_provincia$rezagos_observados +
        tabla_oportunidad_sexo_provincia$rezagos_ausentes
    ) ==
    nrow(cuadricula_rezagos_sexo_provincia),
  
  ausencias_no_imputadas =
    all(
      is.na(
        auditoria_cobertura_sexo_provincia$defunciones[
          !auditoria_cobertura_sexo_provincia$combinacion_observada
        ]
      )
    ),
  
  valores_observados_validos =
    all(
      is.finite(
        auditoria_cobertura_sexo_provincia$defunciones[
          auditoria_cobertura_sexo_provincia$combinacion_observada
        ]
      )
    ),
  
  porcentajes_en_rango =
    all(
      tabla_oportunidad_sexo_provincia$pct_tardias_observado[
        !is.na(tabla_oportunidad_sexo_provincia$pct_tardias_observado)
      ] >= 0 &
        tabla_oportunidad_sexo_provincia$pct_tardias_observado[
          !is.na(tabla_oportunidad_sexo_provincia$pct_tardias_observado)
        ] <= 100
    ),
  
  porcentaje_requiere_rezago_cero =
    all(
      is.na(
        tabla_oportunidad_sexo_provincia$pct_tardias_observado[
          !tabla_oportunidad_sexo_provincia$rezago_cero_observado
        ]
      )
    ),
  
  anio_2025_sin_porcentajes =
    all(
      is.na(
        tabla_oportunidad_sexo_provincia$pct_tardias_observado[
          tabla_oportunidad_sexo_provincia$anio_defuncion == 2025
        ]
      )
    ),
  
  resumen_sexo_correcto =
    nrow(resumen_oportunidad_por_sexo) == 22,
  
  resumen_provincia_correcto =
    nrow(resumen_oportunidad_por_provincia) == 77
)


cat(
  "\nVALIDACIONES DE LA SECCIÓN 15.2\n"
)

print(
  comprobaciones_sexo_provincia
)

stopifnot(
  all(comprobaciones_sexo_provincia)
)


# ============================================================
# 15.2.13. AUDITORÍA DE GUIONES, CEROS EXPLÍCITOS Y
#          COMBINACIONES AUSENTES
# ============================================================

# OBJETIVO:
#
# Auditar la representación de las combinaciones de año de
# ocurrencia, año de declaración, sexo y provincia.
#
# Se distinguen cuatro situaciones:
#
# 1. Valor positivo:
#    La celda está representada y contiene defunciones.
#
# 2. Guion:
#    La celda está representada mediante un guion (-).
#    En defunciones se convierte numéricamente en cero,
#    pero estado_registro conserva la categoría "guion".
#
# 3. Cero explícito:
#    Se comprueba si existe algún cero explícito en los
#    archivos originales. Según las validaciones previas,
#    no existen ceros explícitos.
#
# 4. Combinación ausente:
#    No existe una fila para esa combinación en los datos.
#    No debe interpretarse como cero ni como guion.
#
# Las categorías estructuralmente no disponibles se excluyen
# mediante categoria_disponible.
#
# IMPORTANTE:
# Esta auditoría es descriptiva. No modifica datos_completos
# ni las matrices construidas en las secciones anteriores.


# ------------------------------------------------------------
# 15.2.13.1. Preparar la base de auditoría
# ------------------------------------------------------------

base_auditoria_guiones <- datos_completos %>%
  filter(
    universo == anio_declaracion,
    anio_declaracion %in% anios_ocurrencia,
    anio_defuncion %in% anios_ocurrencia,
    categoria_disponible,
    retraso_declaracion >= 0,
    retraso_declaracion <= 9
  ) %>%
  mutate(
    estado_celda = case_when(
      estado_registro == "guion" ~ "Guion",
      estado_registro == "cero_explicito" ~ "Cero explícito",
      estado_registro == "positivo" ~ "Valor positivo",
      TRUE ~ "Valor no clasificable"
    )
  )


# ------------------------------------------------------------
# 15.2.13.2. Auditoría nacional por combinación temporal
# ------------------------------------------------------------

auditoria_nacional_guiones <- base_auditoria_guiones %>%
  group_by(
    anio_defuncion,
    anio_declaracion,
    retraso_declaracion
  ) %>%
  summarise(
    n_celdas_representadas = n(),
    n_guiones = sum(estado_celda == "Guion"),
    n_ceros_explicitos = sum(estado_celda == "Cero explícito"),
    n_celdas_positivas = sum(estado_celda == "Valor positivo"),
    n_no_clasificables = sum(
      estado_celda == "Valor no clasificable"
    ),
    defunciones_observadas = sum(defunciones, na.rm = TRUE),
    .groups = "drop"
  )


# ------------------------------------------------------------
# 15.2.13.3. Construir la cuadrícula temporal nacional
# ------------------------------------------------------------

cuadricula_auditoria_nacional <- tidyr::crossing(
  anio_defuncion = anios_ocurrencia,
  retraso_declaracion = 0:9
) %>%
  mutate(
    anio_declaracion =
      anio_defuncion + retraso_declaracion
  ) %>%
  filter(
    anio_declaracion <= anio_corte
  )


# ------------------------------------------------------------
# 15.2.13.4. Clasificar la cobertura nacional
# ------------------------------------------------------------

auditoria_nacional_completa <- cuadricula_auditoria_nacional %>%
  left_join(
    auditoria_nacional_guiones,
    by = c(
      "anio_defuncion",
      "anio_declaracion",
      "retraso_declaracion"
    )
  ) %>%
  mutate(
    across(
      c(
        n_celdas_representadas,
        n_guiones,
        n_ceros_explicitos,
        n_celdas_positivas,
        n_no_clasificables
      ),
      ~ tidyr::replace_na(.x, 0L)
    ),
    defunciones_observadas = tidyr::replace_na(
      defunciones_observadas,
      0
    ),
    estado_combinacion = case_when(
      n_celdas_representadas == 0 ~
        "Combinación ausente",
      
      n_no_clasificables > 0 ~
        "Revisar clasificación",
      
      n_celdas_positivas > 0 ~
        "Representada con valores positivos",
      
      n_guiones > 0 &
        n_ceros_explicitos == 0 ~
        "Representada solo con guiones",
      
      n_ceros_explicitos > 0 &
        n_guiones == 0 ~
        "Representada con ceros explícitos",
      
      n_ceros_explicitos > 0 &
        n_guiones > 0 ~
        "Representada con ceros y guiones",
      
      TRUE ~
        "Revisar clasificación"
    )
  )


# ------------------------------------------------------------
# 15.2.13.5. Auditoría por sexo y provincia
# ------------------------------------------------------------

auditoria_sexo_provincia_guiones <- base_auditoria_guiones %>%
  group_by(
    anio_defuncion,
    anio_declaracion,
    retraso_declaracion,
    sexo,
    provincia_id,
    provincia
  ) %>%
  summarise(
    n_celdas_representadas = n(),
    n_guiones = sum(estado_celda == "Guion"),
    n_ceros_explicitos = sum(estado_celda == "Cero explícito"),
    n_celdas_positivas = sum(estado_celda == "Valor positivo"),
    n_no_clasificables = sum(
      estado_celda == "Valor no clasificable"
    ),
    defunciones_observadas = sum(defunciones, na.rm = TRUE),
    .groups = "drop"
  )



# ------------------------------------------------------------
# 15.2.13.6. Construir la cuadrícula por sexo y provincia
# ------------------------------------------------------------

cuadricula_auditoria_sexo_provincia <- tidyr::crossing(
  anio_defuncion = anios_ocurrencia,
  retraso_declaracion = 0:9,
  sexo = sexos_esperados,
  provincias_esperadas
) %>%
  mutate(
    anio_declaracion =
      anio_defuncion + retraso_declaracion
  ) %>%
  filter(
    anio_declaracion <= anio_corte
  )



# ------------------------------------------------------------
# 15.2.13.7. Clasificar la cobertura por sexo y provincia
# ------------------------------------------------------------

auditoria_sexo_provincia_completa <-
  cuadricula_auditoria_sexo_provincia %>%
  left_join(
    auditoria_sexo_provincia_guiones,
    by = c(
      "anio_defuncion",
      "anio_declaracion",
      "retraso_declaracion",
      "sexo",
      "provincia_id",
      "provincia"
    )
  ) %>%
  mutate(
    across(
      c(
        n_celdas_representadas,
        n_guiones,
        n_ceros_explicitos,
        n_celdas_positivas,
        n_no_clasificables
      ),
      ~ tidyr::replace_na(.x, 0L)
    ),
    defunciones_observadas = tidyr::replace_na(
      defunciones_observadas,
      0
    ),
    estado_combinacion = case_when(
      n_celdas_representadas == 0 ~
        "Combinación ausente",
      
      n_no_clasificables > 0 ~
        "Revisar clasificación",
      
      n_celdas_positivas > 0 ~
        "Representada con valores positivos",
      
      n_guiones > 0 &
        n_ceros_explicitos == 0 ~
        "Representada solo con guiones",
      
      n_ceros_explicitos > 0 &
        n_guiones == 0 ~
        "Representada con ceros explícitos",
      
      n_ceros_explicitos > 0 &
        n_guiones > 0 ~
        "Representada con ceros y guiones",
      
      TRUE ~
        "Revisar clasificación"
    )
  )


# ------------------------------------------------------------
# 15.2.13.8. Resumen nacional de estados de las combinaciones
# ------------------------------------------------------------

resumen_auditoria_nacional <- auditoria_nacional_completa %>%
  count(
    estado_combinacion,
    name = "n_combinaciones"
  ) %>%
  arrange(
    estado_combinacion
  )

print(resumen_auditoria_nacional)


# ------------------------------------------------------------
# 15.2.13.9. Resumen por sexo y provincia
# ------------------------------------------------------------

resumen_auditoria_sexo_provincia <-
  auditoria_sexo_provincia_completa %>%
  count(
    sexo,
    provincia,
    estado_combinacion,
    name = "n_combinaciones"
  ) %>%
  arrange(
    provincia,
    sexo,
    estado_combinacion
  )

print(resumen_auditoria_sexo_provincia)


# ------------------------------------------------------------
# 15.2.13.10. Revisar las cuatro combinaciones ausentes
# ------------------------------------------------------------

combinaciones_ausentes_15_1 <- tibble::tribble(
  ~anio_defuncion, ~anio_declaracion, ~retraso_declaracion,
  2015L, 2023L, 8L,
  2016L, 2022L, 6L,
  2018L, 2022L, 4L,
  2018L, 2025L, 7L
)


revision_ausencias_nacional <- combinaciones_ausentes_15_1 %>%
  left_join(
    auditoria_nacional_completa %>%
      select(
        anio_defuncion,
        anio_declaracion,
        retraso_declaracion,
        n_celdas_representadas,
        n_guiones,
        n_ceros_explicitos,
        n_celdas_positivas,
        estado_combinacion
      ),
    by = c(
      "anio_defuncion",
      "anio_declaracion",
      "retraso_declaracion"
    )
  )

print(revision_ausencias_nacional)


# ------------------------------------------------------------
# 15.2.13.11. Revisar las mismas ausencias por sexo y provincia
# ------------------------------------------------------------

revision_ausencias_estratificada <-
  auditoria_sexo_provincia_completa %>%
  semi_join(
    combinaciones_ausentes_15_1,
    by = c(
      "anio_defuncion",
      "anio_declaracion",
      "retraso_declaracion"
    )
  ) %>%
  group_by(
    anio_defuncion,
    anio_declaracion,
    retraso_declaracion
  ) %>%
  summarise(
    n_estratos_esperados = n(),
    n_estratos_con_filas = sum(
      n_celdas_representadas > 0
    ),
    n_estratos_con_positivos = sum(
      n_celdas_positivas > 0
    ),
    n_estratos_con_guiones = sum(
      n_guiones > 0
    ),
    .groups = "drop"
  )

print(revision_ausencias_estratificada)


# ------------------------------------------------------------
# 15.2.13.12. Validaciones de la auditoría
# ------------------------------------------------------------

validaciones_auditoria_guiones <- tibble::tibble(
  validacion = c(
    "Cuadrícula nacional completa",
    "Cuadrícula estratificada completa",
    "Sin duplicados en cuadrícula nacional",
    "Sin duplicados en cuadrícula estratificada",
    "Todas las celdas tienen clasificación válida",
    "Las cuatro ausencias de la sección 15.1 están identificadas",
    "No existen ceros explícitos en los registros originales",
    "Las combinaciones ausentes no se clasifican como guiones"
  ),
  resultado = c(
    nrow(auditoria_nacional_completa) ==
      nrow(cuadricula_auditoria_nacional),
    
    nrow(auditoria_sexo_provincia_completa) ==
      nrow(cuadricula_auditoria_sexo_provincia),
    
    !anyDuplicated(
      auditoria_nacional_completa %>%
        select(
          anio_defuncion,
          anio_declaracion,
          retraso_declaracion
        )
    ),
    
    !anyDuplicated(
      auditoria_sexo_provincia_completa %>%
        select(
          anio_defuncion,
          anio_declaracion,
          retraso_declaracion,
          sexo,
          provincia_id
        )
    ),
    
    !any(
      base_auditoria_guiones$estado_celda ==
        "Valor no clasificable"
    ),
    
    nrow(
      revision_ausencias_nacional
    ) == 4 &&
      all(
        revision_ausencias_nacional$estado_combinacion ==
          "Combinación ausente"
      ),
    
    sum(
      base_auditoria_guiones$estado_celda ==
        "Cero explícito"
    ) == 0,
    
    all(
      auditoria_nacional_completa$estado_combinacion[
        auditoria_nacional_completa$n_celdas_representadas == 0
      ] == "Combinación ausente"
    )
  )
)

print(validaciones_auditoria_guiones)

stopifnot(
  all(validaciones_auditoria_guiones$resultado)
)


# ------------------------------------------------------------
# 15.2.13.13. Resumen final de la auditoría
# ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("AUDITORÍA DE GUIONES Y COMBINACIONES AUSENTES\n")
cat("============================================================\n")

cat(
  "\nCombinaciones temporales nacionales:",
  nrow(auditoria_nacional_completa),
  "\n"
)

cat(
  "Combinaciones temporales estratificadas:",
  nrow(auditoria_sexo_provincia_completa),
  "\n"
)

cat(
  "Celdas originales clasificadas como guion:",
  sum(base_auditoria_guiones$estado_celda == "Guion"),
  "\n"
)

cat(
  "Celdas originales clasificadas como cero explícito:",
  sum(base_auditoria_guiones$estado_celda == "Cero explícito"),
  "\n"
)

cat(
  "Celdas originales con valores positivos:",
  sum(base_auditoria_guiones$estado_celda == "Valor positivo"),
  "\n"
)

cat(
  "Celdas originales no clasificables:",
  sum(
    base_auditoria_guiones$estado_celda ==
      "Valor no clasificable"
  ),
  "\n"
)

cat(
  "Combinaciones temporales ausentes:",
  sum(
    auditoria_nacional_completa$estado_combinacion ==
      "Combinación ausente"
  ),
  "\n"
)

cat("\nValidaciones aprobadas:",
    sum(validaciones_auditoria_guiones$resultado),
    "de",
    nrow(validaciones_auditoria_guiones),
    "\n")

cat("\nSECCIÓN 15.2.13 COMPLETADA\n")


# ------------------------------------------------------------
# 15.2.13.14. PREPARAR LOS DATOS PARA EL GRÁFICO
# ------------------------------------------------------------

datos_grafico_sexo_provincia <-
  tabla_oportunidad_sexo_provincia %>%
  filter(
    !is.na(pct_tardias_observado)
  ) %>%
  mutate(
    estado_grafico = case_when(
      interpretacion_porcentaje ==
        "Provisional: cobertura incompleta" ~
        "Cobertura incompleta",
      
      interpretacion_porcentaje ==
        "Porcentaje observado: cobertura completa" ~
        "Cobertura completa",
      
      TRUE ~
        "No clasificable"
    )
  ) %>%
  filter(
    estado_grafico != "No clasificable"
  ) %>%
  arrange(
    provincia_id,
    sexo,
    anio_defuncion
  )

# Comprobación de la base para el gráfico

stopifnot(
  nrow(datos_grafico_sexo_provincia) > 0,
  all(is.finite(
    datos_grafico_sexo_provincia$pct_tardias_observado
  )),
  all(
    datos_grafico_sexo_provincia$pct_tardias_observado >= 0
  )
)

cat(
  "\nFilas disponibles para el gráfico:",
  nrow(datos_grafico_sexo_provincia),
  "\n"
)


# ------------------------------------------------------------
# 15.2.14. GRÁFICO POR SEXO Y PROVINCIA
# ------------------------------------------------------------

grafico_oportunidad_sexo_provincia <-
  ggplot2::ggplot(
    datos_grafico_sexo_provincia,
    ggplot2::aes(
      x = anio_defuncion,
      y = pct_tardias_observado,
      color = sexo,
      group = sexo
    )
  ) +
  
  ggplot2::geom_line(
    linewidth = 0.6
  ) +
  
  ggplot2::geom_point(
    ggplot2::aes(
      shape = estado_grafico
    ),
    size = 2.5
  ) +
  
  
  ggplot2::facet_wrap(
    ~ provincia,
    ncol = 2,
    axes = "all_x",
    axis.labels = "all_x"
  ) +
  
  ggplot2::scale_x_continuous(
    breaks = anios_ocurrencia,
    limits = range(anios_ocurrencia)
  ) +
  
  ggplot2::scale_y_continuous(
    limits = c(0, NA),
    expand = ggplot2::expansion(
      mult = c(0, 0.08)
    )
  ) +
  
  ggplot2::labs(
    
    title =
      "Oportunidad del registro de defunciones por sexo y provincia",
    
    subtitle =
      "Porcentaje de defunciones tardías observadas según año de ocurrencia",
    
    x = "Año de ocurrencia",
    
    y = "Defunciones tardías observadas (%)",
    
    color = "Sexo",
    
    shape = "Estado del análisis",
    
    caption =
      paste(
        "Nota: los porcentajes con cobertura incompleta",
        "son provisionales.",
        "2025 no presenta porcentaje por falta de seguimiento posterior.",
        "Las combinaciones ausentes no se imputaron como cero."
      )
  ) +
  
  ggplot2::theme_minimal(
    base_size = 11
  ) +
  
  ggplot2::theme(
    
    plot.title = ggplot2::element_text(
      face = "bold"
    ),
    
    axis.text.x = ggplot2::element_text(
      angle = 45,
      hjust = 1
    ),
    
    legend.position = "bottom",
    
    strip.text = ggplot2::element_text(
      face = "bold"
    ),
    
    plot.caption = ggplot2::element_text(
      hjust = 0,
      size = 9
    )
  )


print(
  grafico_oportunidad_sexo_provincia
)


# ------------------------------------------------------------
# 15.2.15. GUARDAR EL GRÁFICO
# ------------------------------------------------------------

ggplot2::ggsave(
  filename =
    "oportunidad_registro_defunciones_sexo_provincia.png",
  plot = grafico_oportunidad_sexo_provincia,
  width = 14,
  height = 18,
  units = "in",
  dpi = 300
)





# 
# ------------------------------------------------------------

# ------------------------------------------------------------
# 15.2.14.1. PREPARE DATA FOR THE GRAPH
# ------------------------------------------------------------

datos_grafico_sexo_provincia_en <-
  tabla_oportunidad_sexo_provincia %>%
  filter(
    !is.na(pct_tardias_observado)
  ) %>%
  mutate(
    estado_grafico_en = case_when(
      estado_cobertura == "Cobertura incompleta" ~
        "Incomplete coverage",
      
      estado_cobertura == "Cobertura completa" ~
        "Complete coverage",
      
      TRUE ~ "Not classified"
    )
  ) %>%
  filter(
    estado_grafico_en != "Not classified"
  ) %>%
  arrange(
    provincia_id,
    sexo,
    anio_defuncion
  )

# Data validation

stopifnot(
  nrow(datos_grafico_sexo_provincia_en) > 0,
  all(
    is.finite(
      datos_grafico_sexo_provincia_en$pct_tardias_observado
    )
  ),
  all(
    datos_grafico_sexo_provincia_en$pct_tardias_observado >= 0
  )
)


# ------------------------------------------------------------
# 15.2.14.2. CREATE THE GRAPH
# ------------------------------------------------------------

grafico_oportunidad_sexo_provincia_en <-
  ggplot2::ggplot(
    datos_grafico_sexo_provincia_en,
    ggplot2::aes(
      x = anio_defuncion,
      y = pct_tardias_observado,
      color = sexo,
      group = sexo
    )
  ) +
  
  ggplot2::geom_line(
    linewidth = 0.6
  ) +
  
  ggplot2::geom_point(
    ggplot2::aes(
      shape = estado_grafico_en
    ),
    size = 2.5
  ) +
  
  
  ggplot2::facet_wrap(
    ~ provincia,
    ncol = 2,
    axes = "all_x",
    axis.labels = "all_x"
  ) +
  
  
  ggplot2::scale_x_continuous(
    breaks = anios_ocurrencia,
    limits = range(anios_ocurrencia)
  ) +
  
  ggplot2::scale_y_continuous(
    limits = c(0, NA),
    expand = ggplot2::expansion(
      mult = c(0, 0.08)
    )
  ) +
  
  ggplot2::labs(
    
    title =
      "Timeliness of Death Registration by Sex and Province",
    
    subtitle =
      "Percentage of observed late-registered deaths by year of occurrence",
    
    x = "Year of occurrence",
    
    y = "Observed late-registered deaths (%)",
    
    color = "Sex",
    
    shape = "Analysis status",
    
    caption =
      paste(
        "Note: percentages with incomplete coverage",
        "are provisional.",
        "No percentage is reported for 2025 due to the lack",
        "of subsequent follow-up.",
        "Missing combinations were not imputed as zero."
      )
  ) +
  
  ggplot2::theme_minimal(
    base_size = 11
  ) +
  
  ggplot2::theme(
    
    plot.title = ggplot2::element_text(
      face = "bold"
    ),
    
    axis.text.x = ggplot2::element_text(
      angle = 45,
      hjust = 1
    ),
    
    legend.position = "bottom",
    
    strip.text = ggplot2::element_text(
      face = "bold"
    ),
    
    plot.caption = ggplot2::element_text(
      hjust = 0,
      size = 9
    )
  )

# Display graph

print(grafico_oportunidad_sexo_provincia_en)




# ------------------------------------------------------------
# 15.2.16. CIERRE
# ------------------------------------------------------------

cat(
  "\nSECCIÓN 15.2 COMPLETADA.\n",
  "Se construyó la matriz de referencia por sexo y provincia.\n",
  "Se auditó la cobertura de los rezagos esperados.\n",
  "Se calcularon los porcentajes observados de registro tardío.\n",
  "Las combinaciones ausentes se conservaron como ausencias.\n",
  "Se generó el gráfico comparativo por sexo y provincia.\n",
  sep = ""
)

# ============================================================
# FIN DE LA SECCIÓN 15.2
# ============================================================


# ============================================================
# AUDITORÍA HISTÓRICA DE LA VENTANA DE REZAGOS 0–9
# Cohortes de ocurrencia 2000–2025
# ============================================================

# 1. Definir el último año disponible.

anio_corte <- max(
  datos_completos$anio_declaracion,
  na.rm = TRUE
)


# 2. Buscar defunciones con retrasos superiores a nueve años.

auditoria_rezagos_mayores_9 <-
  datos_completos %>%
  filter(
    universo == anio_declaracion,
    anio_defuncion %in% 2000:anio_corte,
    retraso_declaracion > 9,
    categoria_disponible,
    defunciones > 0
  ) %>%
  group_by(
    anio_defuncion,
    retraso_declaracion
  ) %>%
  summarise(
    defunciones = sum(defunciones, na.rm = TRUE),
    .groups = "drop"
  )

print(
  auditoria_rezagos_mayores_9,
  n = Inf
)


# 3. Identificar los rezagos temporalmente observables
#    en cada cohorte de ocurrencia.

rezagos_historicos_observados <-
  datos_completos %>%
  filter(
    universo == anio_declaracion,
    anio_defuncion %in% 2000:anio_corte,
    retraso_declaracion %in% 0:9
  ) %>%
  distinct(
    anio_defuncion,
    retraso_declaracion
  ) %>%
  mutate(rezago_observado = TRUE)


# 4. Construir la matriz de rezagos esperados.
#    Las cohortes recientes solo incluyen los rezagos
#    que podían observarse hasta el año de corte.

rezagos_esperados_historicos <-
  tidyr::crossing(
    anio_defuncion = 2000:anio_corte,
    retraso_declaracion = 0:9
  ) %>%
  filter(
    anio_defuncion + retraso_declaracion <= anio_corte
  )


# 5. Calcular cobertura temporal por cohorte.

cobertura_ventana_historica <-
  rezagos_esperados_historicos %>%
  left_join(
    rezagos_historicos_observados,
    by = c(
      "anio_defuncion",
      "retraso_declaracion"
    )
  ) %>%
  group_by(anio_defuncion) %>%
  summarise(
    rezagos_esperados = n(),
    rezagos_observados = sum(
      !is.na(rezago_observado)
    ),
    rezagos_ausentes =
      rezagos_esperados - rezagos_observados,
    seguimiento_disponible =
      anio_corte - first(anio_defuncion),
    ventana_0_9_completa =
      seguimiento_disponible >= 9,
    .groups = "drop"
  )

print(
  cobertura_ventana_historica,
  n = Inf
)


# 6. Identificar las combinaciones temporales ausentes.

rezagos_ausentes_historicos <-
  rezagos_esperados_historicos %>%
  anti_join(
    rezagos_historicos_observados,
    by = c(
      "anio_defuncion",
      "retraso_declaracion"
    )
  )

print(
  rezagos_ausentes_historicos,
  n = Inf
)



# ============================================================
# 15.3. PROVINCIAL TRENDS IN LATE DEATH REGISTRATION
#       2015–2025
# ============================================================

# OBJECTIVE:
#
# Examine provincial trends in the observed percentage of
# late-registered deaths and compare them with the national
# reference.
#
# METHODOLOGICAL CRITERIA:
#
# 1. Aggregate male and female death counts within each
#    province and year of occurrence.
# 2. Calculate percentages from aggregated counts, not by
#    averaging sex-specific or provincial percentages.
# 3. Calculate the national reference from pooled counts
#    across all seven provinces.
# 4. Identify estimates with incomplete coverage or less
#    than five years of follow-up as provisional.
# 5. Retain 2025 on the x-axis but do not calculate or plot
#    a percentage for that year.
# 6. Do not interpret missing values as zero.
#
# ============================================================


# ------------------------------------------------------------
# 15.3.1. DEFINE THE ANALYSIS PERIOD AND EXPECTED STRATA
# ------------------------------------------------------------

anios_ocurrencia <- 2015:2025

anio_corte <- max(anios_ocurrencia)

n_sexos_esperados <-
  dplyr::n_distinct(datos_completos$sexo)

n_provincias_esperadas <-
  dplyr::n_distinct(datos_completos$provincia_id)


# ------------------------------------------------------------
# 15.3.2. AGGREGATE BOTH SEXES BY PROVINCE AND YEAR
# ------------------------------------------------------------

resumen_provincial_en <-
  tabla_oportunidad_sexo_provincia %>%
  dplyr::filter(
    anio_defuncion %in% anios_ocurrencia
  ) %>%
  dplyr::group_by(
    anio_defuncion,
    provincia_id,
    provincia
  ) %>%
  dplyr::summarise(
    
    n_sexos = dplyr::n_distinct(sexo),
    
    defunciones_tardias = sum(
      defunciones_tardias,
      na.rm = TRUE
    ),
    
    total_defunciones_observadas = sum(
      total_defunciones_observadas,
      na.rm = TRUE
    ),
    
    rezagos_tardios_observados = sum(
      rezagos_tardios_observados,
      na.rm = TRUE
    ),
    
    rezago_cero_observado = all(
      rezago_cero_observado %in% TRUE
    ),
    
    cobertura_completa = all(
      cobertura_completa %in% TRUE
    ),
    
    anios_seguimiento = if (
      all(is.na(anios_seguimiento))
    ) {
      NA_real_
    } else {
      min(anios_seguimiento, na.rm = TRUE)
    },
    
    .groups = "drop"
    
  ) %>%
  dplyr::mutate(
    
    pct_tardias_observado = dplyr::case_when(
      
      anio_defuncion == anio_corte ~
        NA_real_,
      
      n_sexos != n_sexos_esperados ~
        NA_real_,
      
      !rezago_cero_observado ~
        NA_real_,
      
      rezagos_tardios_observados == 0 ~
        NA_real_,
      
      total_defunciones_observadas <= 0 ~
        NA_real_,
      
      TRUE ~
        100 * defunciones_tardias /
        total_defunciones_observadas
    ),
    
    analysis_status = dplyr::case_when(
      
      anio_defuncion == anio_corte ~
        "No follow-up",
      
      n_sexos != n_sexos_esperados ~
        "Provisional",
      
      !cobertura_completa ~
        "Provisional",
      
      is.na(anios_seguimiento) ~
        "Provisional",
      
      anios_seguimiento < 5 ~
        "Provisional",
      
      TRUE ~
        "At least 5 years of follow-up"
    )
  )


# ------------------------------------------------------------
# 15.3.3. CALCULATE THE NATIONAL REFERENCE
# ------------------------------------------------------------
#
# The national percentage is calculated from pooled counts
# across the seven provinces, rather than from the mean of
# provincial percentages.
#
# ------------------------------------------------------------

referencia_nacional_en <-
  resumen_provincial_en %>%
  dplyr::group_by(
    anio_defuncion
  ) %>%
  dplyr::summarise(
    
    n_provincias = dplyr::n_distinct(
      provincia_id
    ),
    
    defunciones_tardias = sum(
      defunciones_tardias,
      na.rm = TRUE
    ),
    
    total_defunciones_observadas = sum(
      total_defunciones_observadas,
      na.rm = TRUE
    ),
    
    rezagos_tardios_observados = sum(
      rezagos_tardios_observados,
      na.rm = TRUE
    ),
    
    rezago_cero_observado = all(
      rezago_cero_observado %in% TRUE
    ),
    
    cobertura_completa = all(
      cobertura_completa %in% TRUE
    ),
    
    anios_seguimiento = if (
      all(is.na(anios_seguimiento))
    ) {
      NA_real_
    } else {
      min(anios_seguimiento, na.rm = TRUE)
    },
    
    .groups = "drop"
    
  ) %>%
  dplyr::mutate(
    
    pct_tardias_observado = dplyr::case_when(
      
      anio_defuncion == anio_corte ~
        NA_real_,
      
      n_provincias != n_provincias_esperadas ~
        NA_real_,
      
      !rezago_cero_observado ~
        NA_real_,
      
      rezagos_tardios_observados == 0 ~
        NA_real_,
      
      total_defunciones_observadas <= 0 ~
        NA_real_,
      
      TRUE ~
        100 * defunciones_tardias /
        total_defunciones_observadas
    ),
    
    analysis_status = dplyr::case_when(
      
      anio_defuncion == anio_corte ~
        "No follow-up",
      
      n_provincias != n_provincias_esperadas ~
        "Provisional",
      
      !cobertura_completa ~
        "Provisional",
      
      is.na(anios_seguimiento) ~
        "Provisional",
      
      anios_seguimiento < 5 ~
        "Provisional",
      
      TRUE ~
        "At least 5 years of follow-up"
    )
  )


# ------------------------------------------------------------
# 15.3.4. VALIDATE THE AGGREGATED DATA
# ------------------------------------------------------------

stopifnot(
  
  nrow(resumen_provincial_en) > 0,
  
  nrow(referencia_nacional_en) > 0,
  
  all(
    resumen_provincial_en$n_sexos <=
      n_sexos_esperados
  ),
  
  all(
    referencia_nacional_en$n_provincias <=
      n_provincias_esperadas
  ),
  
  all(
    is.na(resumen_provincial_en$pct_tardias_observado) |
      is.finite(resumen_provincial_en$pct_tardias_observado)
  ),
  
  all(
    is.na(referencia_nacional_en$pct_tardias_observado) |
      is.finite(referencia_nacional_en$pct_tardias_observado)
  ),
  
  all(
    is.na(
      resumen_provincial_en$pct_tardias_observado[
        resumen_provincial_en$anio_defuncion == anio_corte
      ]
    )
  ),
  
  all(
    is.na(
      referencia_nacional_en$pct_tardias_observado[
        referencia_nacional_en$anio_defuncion == anio_corte
      ]
    )
  )
)


# ------------------------------------------------------------
# 15.3.5. CREATE THE PROVINCIAL TREND GRAPH
# ------------------------------------------------------------

grafico_tendencias_provinciales_en <-
  
  ggplot2::ggplot(
    
    resumen_provincial_en,
    
    ggplot2::aes(
      x = anio_defuncion,
      y = pct_tardias_observado,
      color = provincia,
      group = provincia
    )
    
  ) +
  
  ggplot2::geom_line(
    linewidth = 0.6,
    na.rm = TRUE
  ) +
  
  ggplot2::geom_point(
    ggplot2::aes(
      shape = analysis_status
    ),
    size = 2.5,
    na.rm = TRUE
  ) +
  
  # National reference
  
  ggplot2::geom_line(
    data = referencia_nacional_en,
    
    ggplot2::aes(
      x = anio_defuncion,
      y = pct_tardias_observado,
      group = 1
    ),
    
    inherit.aes = FALSE,
    color = "black",
    linetype = "dashed",
    linewidth = 1.1,
    na.rm = TRUE
  ) +
  
  ggplot2::geom_point(
    data = referencia_nacional_en,
    
    ggplot2::aes(
      x = anio_defuncion,
      y = pct_tardias_observado
    ),
    
    inherit.aes = FALSE,
    color = "black",
    size = 2.5,
    na.rm = TRUE
  ) +
  
  ggplot2::scale_x_continuous(
    breaks = anios_ocurrencia,
    limits = range(anios_ocurrencia)
  ) +
  
  ggplot2::scale_y_continuous(
    limits = c(0, NA),
    expand = ggplot2::expansion(
      mult = c(0, 0.08)
    )
  ) +
  
  ggplot2::scale_shape_manual(
    values = c(
      "At least 5 years of follow-up" = 16,
      "Provisional" = 1,
      "No follow-up" = 4
    ),
    breaks = c(
      "At least 5 years of follow-up",
      "Provisional"
    )
  ) +
  
  ggplot2::labs(
    
    title =
      "Costa Rica, 2015–2025: Provincial trends in late death registration",
    
    subtitle =
      paste(
        "Provincial percentages by year of occurrence;",
        "dashed black line represents the national reference"
      ),
    
    x = "Year of occurrence",
    
    y = "Observed late-registered deaths (%)",
    
    color = "Province",
    
    shape = "Follow-up status",
    
    caption =
      paste(
        "Note: percentages are calculated from pooled male and female counts.",
        "Open circles identify provisional estimates due to incomplete coverage",
        "and/or less than five years of follow-up.",
        "No percentage is reported for 2025 because subsequent follow-up is unavailable.",
        "Missing occurrence–declaration combinations were not imputed as zero."
      )
  ) +
  
  ggplot2::theme_minimal(
    base_size = 11
  ) +
  
  ggplot2::theme(
    
    plot.title = ggplot2::element_text(
      face = "bold"
    ),
    
    legend.position = "bottom",
    
    plot.caption = ggplot2::element_text(
      hjust = 0,
      size = 9
    )
  )


# Display graph

print(
  grafico_tendencias_provinciales_en
)


# ------------------------------------------------------------
# 15.3.6. SAVE THE GRAPH
# ------------------------------------------------------------

ggplot2::ggsave(
  
  filename =
    "Costa_Rica_provincial_trends_late_registration_2015_2025.png",
  
  plot = grafico_tendencias_provinciales_en,
  
  width = 14,
  height = 8,
  units = "in",
  dpi = 300
)


# ============================================================
# END OF SECTION 15.3
# ============================================================



# ============================================================
# 15.4. CONCILIACIÓN DE LOS TOTALES NACIONALES Y PROVINCIALES
# ============================================================
#
# OBJETIVO:
#
# Comprobar que la agregación de las defunciones observadas
# en las siete provincias y ambos sexos reproduce los totales
# nacionales para cada año de ocurrencia (2015–2025).
#
# Se comparan:
# 1. Defunciones oportunas (rezago 0).
# 2. Defunciones tardías (rezagos > 0).
# 3. Total de defunciones observadas.
#
# IMPORTANTE:
#
# - Se utilizan únicamente combinaciones observadas.
# - Las combinaciones ausentes no se convierten en ceros.
# - No se promedian porcentajes.
# - La conciliación se realiza sobre los conteos.
#
# ============================================================


# ------------------------------------------------------------
# 15.4.1. RESUMIR LOS CONTEOS NACIONALES
# ------------------------------------------------------------

resumen_nacional_conciliacion <-
  auditoria_cobertura_cepal %>%
  dplyr::group_by(
    anio_defuncion
  ) %>%
  dplyr::summarise(
    
    defunciones_oportunas_nacional = sum(
      defunciones[
        combinacion_observada & retraso == 0
      ],
      na.rm = TRUE
    ),
    
    defunciones_tardias_nacional = sum(
      defunciones[
        combinacion_observada & retraso > 0
      ],
      na.rm = TRUE
    ),
    
    total_defunciones_nacional = sum(
      defunciones[
        combinacion_observada
      ],
      na.rm = TRUE
    ),
    
    .groups = "drop"
  )


# ------------------------------------------------------------
# 15.4.2. AGREGAR LOS CONTEOS PROVINCIALES
# ------------------------------------------------------------

resumen_provincial_conciliacion <-
  auditoria_cobertura_sexo_provincia %>%
  dplyr::group_by(
    anio_defuncion
  ) %>%
  dplyr::summarise(
    
    provincias_representadas = dplyr::n_distinct(
      provincia_id[combinacion_observada]
    ),
    
    sexos_representados = dplyr::n_distinct(
      sexo[combinacion_observada]
    ),
    
    estratos_con_rezago_cero = dplyr::n_distinct(
      paste(
        sexo[
          combinacion_observada & retraso == 0
        ],
        provincia_id[
          combinacion_observada & retraso == 0
        ],
        sep = "_"
      )
    ),
    
    defunciones_oportunas_provincial = sum(
      defunciones[
        combinacion_observada & retraso == 0
      ],
      na.rm = TRUE
    ),
    
    defunciones_tardias_provincial = sum(
      defunciones[
        combinacion_observada & retraso > 0
      ],
      na.rm = TRUE
    ),
    
    total_defunciones_provincial = sum(
      defunciones[
        combinacion_observada
      ],
      na.rm = TRUE
    ),
    
    .groups = "drop"
  )


# ------------------------------------------------------------
# 15.4.3. COMPARAR LOS TOTALES
# ------------------------------------------------------------

conciliacion_nacional_provincial <-
  resumen_nacional_conciliacion %>%
  dplyr::full_join(
    resumen_provincial_conciliacion,
    by = "anio_defuncion"
  ) %>%
  dplyr::mutate(
    
    diferencia_oportunas =
      defunciones_oportunas_provincial -
      defunciones_oportunas_nacional,
    
    diferencia_tardias =
      defunciones_tardias_provincial -
      defunciones_tardias_nacional,
    
    diferencia_total =
      total_defunciones_provincial -
      total_defunciones_nacional,
    
    estado_conciliacion = dplyr::case_when(
      
      is.na(diferencia_oportunas) |
        is.na(diferencia_tardias) |
        is.na(diferencia_total) ~
        "Incomplete comparison",
      
      diferencia_oportunas == 0 &
        diferencia_tardias == 0 &
        diferencia_total == 0 ~
        "Reconciled",
      
      TRUE ~
        "Difference detected"
    )
  ) %>%
  dplyr::arrange(
    anio_defuncion
  )


# ------------------------------------------------------------
# 15.4.4. MOSTRAR LOS RESULTADOS
# ------------------------------------------------------------

cat(
  "\nCONCILIACIÓN DE TOTALES NACIONALES Y PROVINCIALES\n"
)

print(
  conciliacion_nacional_provincial,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------
# 15.4.5. VALIDAR LA CONCILIACIÓN
# ------------------------------------------------------------

n_estratos_esperados <-
  dplyr::n_distinct(datos_completos$sexo) *
  dplyr::n_distinct(datos_completos$provincia_id)


validaciones_conciliacion <- c(
  
  # Se incluyen los once años de ocurrencia.
  once_anios_presentes =
    nrow(conciliacion_nacional_provincial) == 11,
  
  # Se encuentran representados los dos sexos.
  dos_sexos_representados =
    all(
      conciliacion_nacional_provincial$sexos_representados == 2
    ),
  
  # Se encuentran representadas las siete provincias.
  siete_provincias_representadas =
    all(
      conciliacion_nacional_provincial$provincias_representadas == 7
    ),
  
  # Los catorce estratos sexo-provincia tienen rezago cero.
  estratos_con_rezago_cero =
    all(
      conciliacion_nacional_provincial$estratos_con_rezago_cero ==
        n_estratos_esperados
    ),
  
  # Coinciden las defunciones oportunas.
  defunciones_oportunas_conciliadas =
    all(
      conciliacion_nacional_provincial$diferencia_oportunas == 0
    ),
  
  # Coinciden las defunciones tardías.
  defunciones_tardias_conciliadas =
    all(
      conciliacion_nacional_provincial$diferencia_tardias == 0
    ),
  
  # Coinciden los totales de defunciones observadas.
  totales_generales_conciliados =
    all(
      conciliacion_nacional_provincial$diferencia_total == 0
    )
)


cat(
  "\nVALIDACIONES DE LA CONCILIACIÓN:\n"
)

print(
  validaciones_conciliacion
)


# Detener la ejecución si alguna comprobación falla.

stopifnot(
  all(validaciones_conciliacion)
)


cat(
  "\nCONCILIACIÓN NACIONAL-PROVINCIAL SUPERADA.\n"
)


# ============================================================
# FIN DE LA SECCIÓN 15.4
# ============================================================


# ============================================================
# 16. CORRECCIÓN DE DEFUNCIONES POR REGISTRO TARDÍO
#     ADAPTACIÓN ESTRATIFICADA DE LA METODOLOGÍA INEC
# ============================================================


# ------------------------------------------------------------
# 16.1. CONSTRUIR LA MATRIZ ESTRATIFICADA DE OCURRENCIA
#       Y DECLARACIÓN
# ------------------------------------------------------------

# OBJETIVO:
#
# Conservar las defunciones por:
# 1. Año-universo REDATAM.
# 2. Sexo.
# 3. Provincia.
# 4. Edad quinquenal.
# 5. Año de ocurrencia.
# 6. Año de declaración.
#
# Se utiliza el universo cuyo año coincide con el año
# de declaración, siguiendo el criterio de la sección 14.4.
#
# Se conserva la ventana de rezagos 0-9 años.
#
# Los registros no disponibles se mantienen identificados;
# no se convierten en ceros.


matriz_estratificada_inec <-
  datos_completos %>%
  filter(
    universo == anio_declaracion,
    universo >= 2000,
    universo <= 2025,
    anio_defuncion >= 2000,
    retraso_declaracion >= 0,
    retraso_declaracion <= 9
  ) %>%
  select(
    universo,
    sexo,
    provincia_id,
    provincia,
    edad_quinquenal,
    anio_defuncion,
    anio_declaracion,
    retraso_declaracion,
    categoria_disponible,
    defunciones
  )


# ------------------------------------------------------------
# 16.2. IDENTIFICAR LAS DEFUNCIONES REGISTRADAS
#       OPORTUNAMENTE
# ------------------------------------------------------------

# El denominador de cada razón corresponde a las defunciones
# ocurridas y declaradas en el mismo año.
#
# Por tanto, su universo de referencia coincide con el año
# de ocurrencia.


defunciones_oportunas_estratificadas <-
  matriz_estratificada_inec %>%
  filter(
    retraso_declaracion == 0,
    categoria_disponible
  ) %>%
  select(
    anio_defuncion,
    sexo,
    provincia_id,
    edad_quinquenal,
    universo_base = universo,
    defunciones_oportunas = defunciones
  )


# ------------------------------------------------------------
# 16.3. CALCULAR LAS RAZONES OBSERVADAS DE REGISTRO TARDÍO
# ------------------------------------------------------------

# Se vincula cada registro tardío con su correspondiente
# denominador oportuno, manteniendo el mismo:
# año de ocurrencia, sexo, provincia y grupo de edad.
#
# El universo del numerador se conserva como dimensión
# explícita del factor.
#
# Si el denominador es cero o no está disponible,
# la razón queda sin estimar.


factores_registro_observados <-
  matriz_estratificada_inec %>%
  left_join(
    defunciones_oportunas_estratificadas,
    by = c(
      "anio_defuncion",
      "sexo",
      "provincia_id",
      "edad_quinquenal"
    )
  ) %>%
  mutate(
    razon_observada = case_when(
      
      !categoria_disponible ~ NA_real_,
      
      is.na(defunciones_oportunas) ~ NA_real_,
      
      defunciones_oportunas <= 0 ~ NA_real_,
      
      TRUE ~
        defunciones / defunciones_oportunas
      
    )
  ) %>%
  arrange(
    sexo,
    provincia_id,
    edad_quinquenal,
    anio_defuncion,
    retraso_declaracion
  )


# ------------------------------------------------------------
# 16.4. RESUMEN DE LAS RAZONES OBSERVADAS POR REZAGO
# ------------------------------------------------------------

resumen_factores_observados <-
  factores_registro_observados %>%
  group_by(retraso_declaracion) %>%
  summarise(
    
    celdas = n(),
    
    razones_calculables = sum(
      !is.na(razon_observada)
    ),
    
    razones_no_estimables = sum(
      is.na(razon_observada)
    ),
    
    .groups = "drop"
    
  ) %>%
  arrange(retraso_declaracion)


# ------------------------------------------------------------
# 16.5. PRESENTAR RESULTADOS
# ------------------------------------------------------------

cat(
  "\nRESUMEN DE FACTORES OBSERVADOS POR REZAGO\n"
)

print(
  resumen_factores_observados,
  n = Inf,
  width = Inf
)


cat(
  "\nPRIMEROS REGISTROS DE LOS FACTORES ESTRATIFICADOS\n"
)

print(
  factores_registro_observados,
  n = 20,
  width = Inf
)


# ------------------------------------------------------------
# 16.6. DIAGNÓSTICO DE LAS RAZONES OBSERVADAS
# ------------------------------------------------------------

diagnostico_factores_observados <-
  factores_registro_observados %>%
  group_by(retraso_declaracion) %>%
  summarise(
    
    total_celdas = n(),
    
    razones_positivas = sum(
      !is.na(razon_observada) &
        razon_observada > 0
    ),
    
    razones_cero = sum(
      !is.na(razon_observada) &
        razon_observada == 0
    ),
    
    sin_denominador = sum(
      is.na(defunciones_oportunas)
    ),
    
    denominador_cero = sum(
      !is.na(defunciones_oportunas) &
        defunciones_oportunas == 0
    ),
    
    registros_no_disponibles = sum(
      !categoria_disponible
    ),
    
    razones_no_estimables = sum(
      is.na(razon_observada)
    ),
    
    .groups = "drop"
    
  ) %>%
  arrange(retraso_declaracion)


print(
  diagnostico_factores_observados,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------
# 16.6. DIAGNÓSTICO DE LAS RAZONES OBSERVADAS
# ------------------------------------------------------------

diagnostico_factores_observados <-
  factores_registro_observados %>%
  group_by(retraso_declaracion) %>%
  summarise(

    total_celdas = n(),

    razones_positivas = sum(
      !is.na(razon_observada) &
        razon_observada > 0
    ),

    razones_cero = sum(
      !is.na(razon_observada) &
        razon_observada == 0
    ),

    sin_denominador = sum(
      is.na(defunciones_oportunas)
    ),

    denominador_cero = sum(
      !is.na(defunciones_oportunas) &
        defunciones_oportunas == 0
    ),

    registros_no_disponibles = sum(
      !categoria_disponible
    ),

    razones_no_estimables = sum(
      is.na(razon_observada)
    ),

    .groups = "drop"

  ) %>%
  arrange(retraso_declaracion)


print(
  diagnostico_factores_observados,
  n = Inf,
  width = Inf
)


# ============================================================
# 16.7. DIAGNÓSTICO DE LOS TRES ANTECEDENTES
#       PARA LA ESTIMACIÓN ARMÓNICA
# ============================================================

# ------------------------------------------------------------
# 16.7.1. DEFINIR LAS COMBINACIONES OBJETIVO
# ------------------------------------------------------------

estratos_correccion <-
  factores_registro_observados %>%
  distinct(
    sexo,
    provincia_id,
    provincia,
    edad_quinquenal
  )


combinaciones_objetivo_armonico <-
  tidyr::crossing(
    estratos_correccion,
    anio_defuncion_objetivo = 2015:2025,
    retraso_declaracion = 1:9
  ) %>%
  mutate(
    universo_objetivo =
      anio_defuncion_objetivo +
      retraso_declaracion
  )


# ------------------------------------------------------------
# 16.7.2. IDENTIFICAR SI EL FACTOR OBJETIVO ESTÁ OBSERVADO
# ------------------------------------------------------------

factores_objetivo_observados <-
  factores_registro_observados %>%
  filter(
    retraso_declaracion >= 1
  ) %>%
  transmute(
    sexo,
    provincia_id,
    edad_quinquenal,
    anio_defuncion_objetivo = anio_defuncion,
    retraso_declaracion,
    registro_objetivo = TRUE,
    razon_objetivo = razon_observada
  )


combinaciones_objetivo_armonico <-
  combinaciones_objetivo_armonico %>%
  left_join(
    factores_objetivo_observados,
    by = c(
      "sexo",
      "provincia_id",
      "edad_quinquenal",
      "anio_defuncion_objetivo",
      "retraso_declaracion"
    )
  ) %>%
  mutate(
    estado_objetivo = case_when(
      
      !is.na(razon_objetivo) ~
        "Factor observado",
      
      universo_objetivo > 2025 ~
        "Pendiente de universo futuro",
      
      !is.na(registro_objetivo) ~
        "Registro presente, razón no calculable",
      
      TRUE ~
        "Sin registro en matriz"
    )
  )


# ------------------------------------------------------------
# 16.7.3. RECUPERAR LOS TRES ANTECEDENTES MÁS RECIENTES
# ------------------------------------------------------------

# Se consideran únicamente razones calculables.
#
# Los valores iguales a cero se conservan como observaciones.
#
# Los años antecedentes deben ser anteriores al año objetivo.
#
# El universo de cada antecedente corresponde a:
# año de ocurrencia antecedente + rezago.


antecedentes_candidatos_armonico <-
  tidyr::crossing(
    combinaciones_objetivo_armonico,
    anio_defuncion_antecedente = 2000:2024
  ) %>%
  filter(
    anio_defuncion_antecedente <
      anio_defuncion_objetivo
  ) %>%
  left_join(
    factores_registro_observados %>%
      filter(
        retraso_declaracion >= 1,
        !is.na(razon_observada)
      ) %>%
      transmute(
        sexo,
        provincia_id,
        edad_quinquenal,
        anio_defuncion_antecedente =
          anio_defuncion,
        retraso_declaracion,
        universo_antecedente = universo,
        razon_antecedente = razon_observada
      ),
    by = c(
      "sexo",
      "provincia_id",
      "edad_quinquenal",
      "anio_defuncion_antecedente",
      "retraso_declaracion"
    )
  )


# Seleccionar los tres años más recientes con razón calculable.

tres_antecedentes_armonico <-
  antecedentes_candidatos_armonico %>%
  filter(
    !is.na(razon_antecedente)
  ) %>%
  group_by(
    sexo,
    provincia_id,
    edad_quinquenal,
    anio_defuncion_objetivo,
    retraso_declaracion
  ) %>%
  arrange(
    desc(anio_defuncion_antecedente),
    .by_group = TRUE
  ) %>%
  slice_head(n = 3) %>%
  ungroup()


# ------------------------------------------------------------
# 16.7.4. RESUMIR LOS ANTECEDENTES POR COMBINACIÓN
# ------------------------------------------------------------

resumen_antecedentes_armonico <-
  tres_antecedentes_armonico %>%
  group_by(
    sexo,
    provincia_id,
    edad_quinquenal,
    anio_defuncion_objetivo,
    retraso_declaracion
  ) %>%
  summarise(
    
    n_antecedentes = n(),
    
    antecedentes_positivos = sum(
      razon_antecedente > 0
    ),
    
    antecedentes_cero = sum(
      razon_antecedente == 0
    ),
    
    anios_antecedentes = paste(
      anio_defuncion_antecedente,
      collapse = ", "
    ),
    
    universos_antecedentes = paste(
      universo_antecedente,
      collapse = ", "
    ),
    
    .groups = "drop"
    
  )


# ------------------------------------------------------------
# 16.7.5. CLASIFICAR LA DISPONIBILIDAD DE ANTECEDENTES
# ------------------------------------------------------------

diagnostico_armonico_detalle <-
  combinaciones_objetivo_armonico %>%
  left_join(
    resumen_antecedentes_armonico,
    by = c(
      "sexo",
      "provincia_id",
      "edad_quinquenal",
      "anio_defuncion_objetivo",
      "retraso_declaracion"
    )
  ) %>%
  mutate(
    
    n_antecedentes = tidyr::replace_na(
      n_antecedentes,
      0L
    ),
    
    antecedentes_positivos = tidyr::replace_na(
      antecedentes_positivos,
      0L
    ),
    
    antecedentes_cero = tidyr::replace_na(
      antecedentes_cero,
      0L
    ),
    
    clasificacion_antecedentes = case_when(
      
      n_antecedentes == 3 &
        antecedentes_positivos == 3 ~
        "Tres antecedentes positivos",
      
      n_antecedentes == 3 &
        antecedentes_cero >= 1 ~
        "Tres antecedentes con uno o más ceros",
      
      TRUE ~
        "Información insuficiente"
    )
  )


# ------------------------------------------------------------
# 16.7.6. RESUMEN GENERAL POR REZAGO Y ESTADO DEL OBJETIVO
# ------------------------------------------------------------

resumen_diagnostico_armonico <-
  diagnostico_armonico_detalle %>%
  count(
    retraso_declaracion,
    estado_objetivo,
    clasificacion_antecedentes,
    name = "combinaciones"
  ) %>%
  arrange(
    retraso_declaracion,
    estado_objetivo,
    clasificacion_antecedentes
  )


# ------------------------------------------------------------
# 16.7.7. PRESENTAR RESULTADOS
# ------------------------------------------------------------

cat(
  "\nDIAGNÓSTICO DE LOS TRES ANTECEDENTES MÁS RECIENTES\n"
)

print(
  resumen_diagnostico_armonico,
  n = Inf,
  width = Inf
)



# ============================================================
# 16.8. DIAGNÓSTICO DE ANTECEDENTES PARA UNIVERSOS FUTUROS
# ============================================================


# ------------------------------------------------------------
# 16.8.1. RESUMIR LOS FACTORES QUE REQUIEREN ESTIMACIÓN
# ------------------------------------------------------------

resumen_futuros_armonico <-
  diagnostico_armonico_detalle %>%
  filter(
    estado_objetivo == "Pendiente de universo futuro"
  ) %>%
  count(
    retraso_declaracion,
    clasificacion_antecedentes,
    name = "combinaciones"
  ) %>%
  arrange(
    retraso_declaracion,
    clasificacion_antecedentes
  )


cat(
  "\nFACTORES PENDIENTES DE UNIVERSOS FUTUROS\n"
)

print(
  resumen_futuros_armonico,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------
# 16.8.2. DISTRIBUIR LOS ANTECEDENTES SEGÚN NÚMERO DE CEROS
# ------------------------------------------------------------

distribucion_ceros_futuros <-
  tres_antecedentes_armonico %>%
  filter(
    universo_objetivo > 2025
  ) %>%
  group_by(
    sexo,
    provincia_id,
    edad_quinquenal,
    anio_defuncion_objetivo,
    retraso_declaracion
  ) %>%
  summarise(
    
    n_antecedentes = n(),
    
    antecedentes_cero = sum(
      razon_antecedente == 0
    ),
    
    antecedentes_positivos = sum(
      razon_antecedente > 0
    ),
    
    razones_historicas = paste(
      signif(razon_antecedente, 4),
      collapse = " | "
    ),
    
    anios_historicos = paste(
      anio_defuncion_antecedente,
      collapse = ", "
    ),
    
    .groups = "drop"
    
  ) %>%
  count(
    retraso_declaracion,
    n_antecedentes,
    antecedentes_cero,
    name = "combinaciones"
  ) %>%
  arrange(
    retraso_declaracion,
    n_antecedentes,
    antecedentes_cero
  )


cat(
  "\nDISTRIBUCIÓN DE CEROS EN LOS ANTECEDENTES FUTUROS\n"
)

print(
  distribucion_ceros_futuros,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------
# 16.8.3. MOSTRAR EJEMPLOS DE COMBINACIONES CON TRES CEROS
# ------------------------------------------------------------

ejemplos_tres_ceros_futuros <-
  tres_antecedentes_armonico %>%
  filter(
    universo_objetivo > 2025
  ) %>%
  group_by(
    sexo,
    provincia_id,
    provincia,
    edad_quinquenal,
    anio_defuncion_objetivo,
    retraso_declaracion
  ) %>%
  filter(
    n() == 3,
    sum(razon_antecedente == 0) == 3
  ) %>%
  ungroup() %>%
  select(
    sexo,
    provincia,
    edad_quinquenal,
    anio_defuncion_objetivo,
    retraso_declaracion,
    anio_defuncion_antecedente,
    universo_antecedente,
    razon_antecedente
  ) %>%
  slice_head(n = 15)


cat(
  "\nEJEMPLOS DE COMBINACIONES CON TRES ANTECEDENTES CERO\n"
)

print(
  ejemplos_tres_ceros_futuros,
  n = Inf,
  width = Inf
)


# ============================================================
# 16.9. CLASIFICACIÓN COMPLETA DE ANTECEDENTES FUTUROS
# ============================================================

# 16.9.1. Recuperar todas las combinaciones futuras
#         y agregar sus antecedentes disponibles.

diagnostico_futuros_completo <-
  combinaciones_objetivo_armonico %>%
  
  filter(
    universo_objetivo > 2025
  ) %>%
  
  left_join(
    resumen_antecedentes_armonico,
    by = c(
      "sexo",
      "provincia_id",
      "edad_quinquenal",
      "anio_defuncion_objetivo",
      "retraso_declaracion"
    )
  ) %>%
  
  mutate(
    
    n_antecedentes = tidyr::replace_na(
      n_antecedentes,
      0L
    ),
    
    antecedentes_positivos = tidyr::replace_na(
      antecedentes_positivos,
      0L
    ),
    
    antecedentes_cero = tidyr::replace_na(
      antecedentes_cero,
      0L
    ),
    
    clasificacion_antecedentes_futura = case_when(
      
      n_antecedentes == 0 ~
        "Sin antecedentes",
      
      n_antecedentes == 1 &
        antecedentes_positivos == 1 ~
        "Un antecedente positivo",
      
      n_antecedentes == 1 &
        antecedentes_cero == 1 ~
        "Un antecedente cero",
      
      n_antecedentes == 2 &
        antecedentes_positivos == 2 ~
        "Dos antecedentes positivos",
      
      n_antecedentes == 2 &
        antecedentes_positivos == 1 ~
        "Un positivo y un cero",
      
      n_antecedentes == 2 &
        antecedentes_cero == 2 ~
        "Dos antecedentes cero",
      
      n_antecedentes == 3 &
        antecedentes_positivos == 3 ~
        "Tres antecedentes positivos",
      
      n_antecedentes == 3 &
        antecedentes_positivos == 2 ~
        "Dos positivos y un cero",
      
      n_antecedentes == 3 &
        antecedentes_positivos == 1 ~
        "Un positivo y dos ceros",
      
      n_antecedentes == 3 &
        antecedentes_cero == 3 ~
        "Tres antecedentes cero",
      
      TRUE ~
        "Revisar clasificación"
    )
  )


# ------------------------------------------------------------
# 16.9.2. RESUMEN POR REZAGO Y CLASIFICACIÓN
# ------------------------------------------------------------

resumen_clasificacion_futuros <-
  diagnostico_futuros_completo %>%
  
  count(
    retraso_declaracion,
    clasificacion_antecedentes_futura,
    name = "combinaciones"
  ) %>%
  
  group_by(
    retraso_declaracion
  ) %>%
  
  mutate(
    porcentaje = round(
      100 * combinaciones / sum(combinaciones),
      2
    )
  ) %>%
  
  ungroup() %>%
  
  arrange(
    retraso_declaracion,
    clasificacion_antecedentes_futura
  )


# ------------------------------------------------------------
# 16.9.3. RESUMEN GLOBAL
# ------------------------------------------------------------

resumen_global_antecedentes_futuros <-
  diagnostico_futuros_completo %>%
  
  count(
    clasificacion_antecedentes_futura,
    name = "combinaciones"
  ) %>%
  
  mutate(
    porcentaje = round(
      100 * combinaciones / sum(combinaciones),
      2
    )
  ) %>%
  
  arrange(
    desc(combinaciones)
  )


# ------------------------------------------------------------
# 16.9.4. VALIDACIONES
# ------------------------------------------------------------

stopifnot(
  
  # Todas las combinaciones futuras están clasificadas.
  nrow(diagnostico_futuros_completo) ==
    nrow(
      combinaciones_objetivo_armonico %>%
        filter(universo_objetivo > 2025)
    ),
  
  # No existen antecedentes negativos.
  all(
    diagnostico_futuros_completo$n_antecedentes >= 0
  ),
  
  # No se seleccionaron más de tres antecedentes.
  all(
    diagnostico_futuros_completo$n_antecedentes <= 3
  ),
  
  # La clasificación no deja casos pendientes.
  !any(
    diagnostico_futuros_completo$clasificacion_antecedentes_futura ==
      "Revisar clasificación"
  ),
  
  # Cada antecedente está clasificado como positivo o cero.
  all(
    diagnostico_futuros_completo$n_antecedentes ==
      diagnostico_futuros_completo$antecedentes_positivos +
      diagnostico_futuros_completo$antecedentes_cero
  )
)


# ------------------------------------------------------------
# 16.9.5. MOSTRAR RESULTADOS
# ------------------------------------------------------------

cat("\nCLASIFICACIÓN POR REZAGO:\n")

print(
  resumen_clasificacion_futuros,
  n = Inf,
  width = Inf
)


cat("\nRESUMEN GLOBAL:\n")

print(
  resumen_global_antecedentes_futuros,
  n = Inf,
  width = Inf
)


cat(
  "\nSECCIÓN 16.9 COMPLETADA.\n"
)



# ============================================================
# 17.1. INCORPORAR BASE DE MORTALIDAD LONGLIVES
# ============================================================

# Archivo quinquenal con redistribución de edad ignorada.
# Esta base todavía NO tiene corrección por registro tardío.

ruta_base_mortalidad <- file.path("C:/Users/mauri/OneDrive - Universidad Nacional de Costa Rica/Documentos/R projects/LONGLIVES 2020-2025/Data/Proccesed/bd_edad_ignorada_distribuida_mortalidad_subnacional/mortalidad_mx_1000_quinquenal_2015_2025_edad_repartida.csv"
)

# Verificar que el archivo exista.
if (!file.exists(ruta_base_mortalidad)) {
  stop(
    "No se encontró la base de mortalidad en: ",
    ruta_base_mortalidad
  )
}

# Leer la base.
base_mortalidad <- readr::read_csv(
  ruta_base_mortalidad,
  show_col_types = FALSE
)

# Verificación estructural inicial.
cat("\nBASE DE MORTALIDAD LONGLIVES\n")
cat("----------------------------\n")

cat("Dimensiones:", nrow(base_mortalidad), "filas ×",
    ncol(base_mortalidad), "columnas\n\n")

cat("Variables disponibles:\n")
print(names(base_mortalidad))

cat("\nEstructura:\n")
dplyr::glimpse(base_mortalidad)

cat("\nAños disponibles:\n")
print(sort(unique(base_mortalidad$anio)))



# ============================================================
# 17.2. COMPARAR CATEGORÍAS DE AMBAS BASES
# ============================================================

cat("\nEDADES - BASE LONGLIVES\n")
print(sort(unique(base_mortalidad$edad)))

cat("\nEDADES - BASE REDATAM\n")
print(unique(datos_completos$edad_quinquenal))

cat("\nPROVINCIAS - BASE LONGLIVES\n")
print(sort(unique(base_mortalidad$region)))

cat("\nPROVINCIAS - BASE REDATAM\n")
print(
  datos_completos %>%
    distinct(provincia_id, provincia) %>%
    arrange(provincia_id)
)

cat("\nSEXOS - BASE LONGLIVES\n")
print(sort(unique(base_mortalidad$sexo)))

cat("\nSEXOS - BASE REDATAM\n")
print(sort(unique(datos_completos$sexo)))




# ============================================================
# 17.3. HOMOLOGAR GRUPOS DE EDAD REDATAM Y LONGLIVES
# ============================================================

datos_completos_homologados <-
  datos_completos %>%
  dplyr::mutate(
    edad_grupo_lgl = dplyr::case_when(
      
      edad_quinquenal == "Menores de 1 año" ~ "0",
      
      edad_quinquenal == "1-4 años" ~ "1-4",
      
      edad_quinquenal == "90-94 años" |
        edad_quinquenal == "95-99 años" |
        edad_quinquenal == "100 y más años" ~ "90 años y más",
      
      edad_quinquenal == "Edad ignorada" ~ "Edad ignorada",
      
      TRUE ~ stringr::str_remove(
        edad_quinquenal,
        " años$"
      )
    )
  )

# Verificar correspondencia de categorías.

cat("\nGRUPOS DE EDAD HOMOLOGADOS\n")

print(
  datos_completos_homologados %>%
    dplyr::distinct(edad_quinquenal, edad_grupo_lgl) %>%
    dplyr::arrange(edad_grupo_lgl)
)

# Identificar categorías sin correspondencia.

cat("\nCATEGORÍAS SIN HOMOLOGAR\n")

print(
  datos_completos_homologados %>%
    dplyr::filter(is.na(edad_grupo_lgl)) %>%
    dplyr::count(edad_quinquenal)
)

# Comparar grupos de edad de ambas bases.

cat("\nGRUPOS DE EDAD LONGLIVES\n")
print(sort(unique(base_mortalidad$edad)))

cat("\nGRUPOS HOMOLOGADOS REDATAM\n")

print(
  sort(unique(
    datos_completos_homologados$edad_grupo_lgl
  ))
)



# ============================================================
# 17.4. IDENTIFICAR ESTRUCTURA TEMPORAL Y DEMOGRÁFICA
#       DE LA BASE REDATAM HOMOLOGADA
# ============================================================

# Resumen de años y universos disponibles.

resumen_estructura_redatam <-
  datos_completos_homologados %>%
  dplyr::summarise(
    universo_min = min(universo, na.rm = TRUE),
    universo_max = max(universo, na.rm = TRUE),
    ocurrencia_min = min(anio_defuncion, na.rm = TRUE),
    ocurrencia_max = max(anio_defuncion, na.rm = TRUE),
    declaracion_min = min(anio_declaracion, na.rm = TRUE),
    declaracion_max = max(anio_declaracion, na.rm = TRUE),
    total_registros = dplyr::n(),
    total_defunciones = sum(defunciones, na.rm = TRUE)
  )

print(resumen_estructura_redatam)

# Combinaciones de universo y año de ocurrencia.

resumen_universo_ocurrencia <-
  datos_completos_homologados %>%
  dplyr::group_by(universo, anio_defuncion) %>%
  dplyr::summarise(
    defunciones = sum(defunciones),
    .groups = "drop"
  ) %>%
  dplyr::arrange(universo, anio_defuncion)

print(resumen_universo_ocurrencia, n = 100)


# ============================================================
# 17.1. CONCILIACIÓN DE TOTALES ENTRE BASES POR UNIVERSO
# ============================================================

# OBJETIVO:
#
# Comparar los totales de defunciones de:
# 1. base_mortalidad: variable anio (universo REDATAM).
# 2. datos_completos: variable universo.
#
# La comparación se realiza por:
# - Universo.
# - Provincia.
# - Sexo.
#
# Se incluyen todos los años de declaración y ocurrencia
# contenidos en cada universo de datos_completos.
#
# No se utiliza anio_declaracion ni anio_defuncion como
# criterio de selección temporal.
#
# En base_mortalidad se utiliza mortalidad, que incorpora
# la redistribución de las defunciones de edad ignorada.


# ------------------------------------------------------------
# 17.1.1. PREPARAR LOS TOTALES DE BASE_MORTALIDAD
# ------------------------------------------------------------

totales_base_mortalidad <-
  base_mortalidad %>%
  dplyr::filter(
    anio >= 2015,
    anio <= 2025,
    sexo != "Total"
  ) %>%
  dplyr::group_by(
    universo = anio,
    provincia = region,
    sexo
  ) %>%
  dplyr::summarise(
    defunciones_base = sum(mortalidad, na.rm = TRUE),
    .groups = "drop"
  )


# ------------------------------------------------------------
# 17.1.2. PREPARAR LOS TOTALES DE DATOS_COMPLETOS
# ------------------------------------------------------------

totales_registro_tardio <-
  datos_completos %>%
  dplyr::filter(
    universo >= 2015,
    universo <= 2025
  ) %>%
  dplyr::group_by(
    universo,
    provincia,
    sexo
  ) %>%
  dplyr::summarise(
    defunciones_redatam = sum(defunciones, na.rm = TRUE),
    .groups = "drop"
  )


# ------------------------------------------------------------
# 17.1.3. COMPARAR AMBAS BASES
# ------------------------------------------------------------

comparacion_universos <-
  dplyr::full_join(
    totales_base_mortalidad,
    totales_registro_tardio,
    by = c(
      "universo",
      "provincia",
      "sexo"
    )
  ) %>%
  dplyr::mutate(
    diferencia = defunciones_base - defunciones_redatam,
    
    diferencia_porcentual = dplyr::if_else(
      !is.na(defunciones_base) &
        !is.na(defunciones_redatam) &
        defunciones_base != 0,
      100 * diferencia / defunciones_base,
      NA_real_
    ),
    
    estado_comparacion = dplyr::case_when(
      is.na(defunciones_base) ~ "Ausente en base_mortalidad",
      is.na(defunciones_redatam) ~ "Ausente en datos_completos",
      diferencia == 0 ~ "Coincide",
      TRUE ~ "Difiere"
    )
  ) %>%
  dplyr::arrange(
    universo,
    provincia,
    sexo
  )


# ------------------------------------------------------------
# 17.1.4. VALIDAR COBERTURA Y RESULTADOS
# ------------------------------------------------------------

# Número de combinaciones por fuente.

cat(
  "\nCombinaciones en base_mortalidad:",
  nrow(totales_base_mortalidad)
)

cat(
  "\nCombinaciones en datos_completos:",
  nrow(totales_registro_tardio)
)

cat(
  "\nCombinaciones en la comparación:",
  nrow(comparacion_universos),
  "\n"
)


# Distribución de resultados de la comparación.

resumen_estado_universos <-
  comparacion_universos %>%
  dplyr::count(
    estado_comparacion,
    name = "combinaciones"
  )

print(resumen_estado_universos)


# ------------------------------------------------------------
# 17.1.5. RESUMEN DE DIFERENCIAS POR UNIVERSO
# ------------------------------------------------------------

resumen_diferencias_universos <-
  comparacion_universos %>%
  dplyr::group_by(universo) %>%
  dplyr::summarise(
    combinaciones = dplyr::n(),
    
    coincidencias = sum(
      estado_comparacion == "Coincide",
      na.rm = TRUE
    ),
    
    diferencias = sum(
      estado_comparacion == "Difiere",
      na.rm = TRUE
    ),
    
    ausentes_base = sum(
      estado_comparacion == "Ausente en base_mortalidad",
      na.rm = TRUE
    ),
    
    ausentes_redatam = sum(
      estado_comparacion == "Ausente en datos_completos",
      na.rm = TRUE
    ),
    
    total_base = sum(
      defunciones_base,
      na.rm = TRUE
    ),
    
    total_redatam = sum(
      defunciones_redatam,
      na.rm = TRUE
    ),
    
    diferencia_total = total_base - total_redatam,
    
    .groups = "drop"
  )

print(resumen_diferencias_universos)


# ------------------------------------------------------------
# 17.1.6. IDENTIFICAR LAS COMBINACIONES CON DIFERENCIAS
# ------------------------------------------------------------

detalle_diferencias_universos <-
  comparacion_universos %>%
  dplyr::filter(
    estado_comparacion != "Coincide"
  ) %>%
  dplyr::arrange(
    universo,
    provincia,
    sexo
  )

print(detalle_diferencias_universos)


# ------------------------------------------------------------
# 17.1.7. VALIDACIÓN FINAL DE EQUIVALENCIA NUMÉRICA
# ------------------------------------------------------------

comparacion_universos <-
  comparacion_universos %>%
  dplyr::mutate(
    coincide_tolerancia = dplyr::if_else(
      !is.na(diferencia),
      abs(diferencia) < 1e-8,
      FALSE
    )
  )

cat(
  "\nCombinaciones equivalentes dentro de tolerancia:",
  sum(comparacion_universos$coincide_tolerancia),
  "de",
  nrow(comparacion_universos),
  "\n"
)

cat(
  "\nDiferencia máxima absoluta:",
  max(abs(comparacion_universos$diferencia), na.rm = TRUE),
  "\n"
)



# ============================================================
# 17.2. EVALUACIÓN DE ALTERNATIVAS PARA FACTORES FUTUROS
# ============================================================

# OBJETIVO:
#
# Comparar dos estimaciones candidatas a partir de los
# antecedentes históricos disponibles:
#
# 1. Media armónica extendida, conservando los ceros.
# 2. Media aritmética, conservando los ceros.
#
# IMPORTANTE:
#
# - Los ceros son observaciones válidas, no faltantes.
# - Las combinaciones sin antecedentes permanecen sin estimar.
# - Se utilizan hasta tres antecedentes disponibles.
# - No se asignan todavía factores definitivos.
# - No se modifica ningún objeto anterior.


# ------------------------------------------------------------
# 17.2.1. CALCULAR ESTIMACIONES CANDIDATAS
# ------------------------------------------------------------

candidatos_factores_futuros <-
  tres_antecedentes_armonico %>%
  filter(
    universo_objetivo > 2025
  ) %>%
  group_by(
    universo_objetivo,
    sexo,
    provincia_id,
    edad_quinquenal,
    anio_defuncion_objetivo,
    retraso_declaracion
  ) %>%
  summarise(
    
    n_antecedentes = n(),
    
    antecedentes_positivos = sum(
      razon_antecedente > 0
    ),
    
    antecedentes_cero = sum(
      razon_antecedente == 0
    ),
    
    # Alternativa 1:
    # Media armónica extendida.
    #
    # Si existe algún cero, el resultado es cero.
    # Si todos son positivos, se aplica la fórmula
    # armónica convencional.
    
    factor_armonico_extendido = if (
      any(razon_antecedente == 0)
    ) {
      0
    } else {
      n() / sum(1 / razon_antecedente)
    },
    
    # Alternativa 2:
    # Media aritmética incluyendo todos los antecedentes.
    
    factor_aritmetico = mean(
      razon_antecedente
    ),
    
    .groups = "drop"
  )


# ------------------------------------------------------------
# 17.2.2. INTEGRAR LA CLASIFICACIÓN DE 16.9
# ------------------------------------------------------------

comparacion_factores_futuros <-
  diagnostico_futuros_completo %>%
  left_join(
    candidatos_factores_futuros,
    by = c(
      "universo_objetivo",
      "sexo",
      "provincia_id",
      "edad_quinquenal",
      "anio_defuncion_objetivo",
      "retraso_declaracion"
    ),
    suffix = c("", "_calculado")
  ) %>%
  mutate(
    
    # Conservar el número de antecedentes de la clasificación
    # original, incluso cuando no existe una estimación.
    
    n_antecedentes = coalesce(
      n_antecedentes_calculado,
      n_antecedentes
    ),
    
    antecedentes_positivos = coalesce(
      antecedentes_positivos_calculado,
      antecedentes_positivos
    ),
    
    antecedentes_cero = coalesce(
      antecedentes_cero_calculado,
      antecedentes_cero
    ),
    
    estado_estimacion = case_when(
      
      n_antecedentes == 0 ~
        "Sin antecedentes",
      
      n_antecedentes < 3 ~
        "Menos de tres antecedentes",
      
      antecedentes_cero == 0 ~
        "Tres antecedentes positivos",
      
      antecedentes_cero > 0 ~
        "Tres antecedentes con ceros",
      
      TRUE ~
        "Revisar"
    )
  ) %>%
  select(
    -n_antecedentes_calculado,
    -antecedentes_positivos_calculado,
    -antecedentes_cero_calculado
  )


# ------------------------------------------------------------
# 17.2.3. RESUMEN DE ESTIMACIONES POR REZAGO
# ------------------------------------------------------------

resumen_alternativas_rezago <-
  comparacion_factores_futuros %>%
  group_by(
    retraso_declaracion,
    estado_estimacion
  ) %>%
  summarise(
    
    combinaciones = n(),
    
    estimaciones_armonicas = sum(
      !is.na(factor_armonico_extendido)
    ),
    
    estimaciones_aritmeticas = sum(
      !is.na(factor_aritmetico)
    ),
    
    media_armonica = mean(
      factor_armonico_extendido,
      na.rm = TRUE
    ),
    
    media_aritmetica = mean(
      factor_aritmetico,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  ) %>%
  arrange(
    retraso_declaracion,
    estado_estimacion
  )


# ------------------------------------------------------------
# 17.2.4. RESUMEN GLOBAL DE LAS ALTERNATIVAS
# ------------------------------------------------------------

resumen_alternativas_global <-
  comparacion_factores_futuros %>%
  summarise(
    
    combinaciones_futuras = n(),
    
    sin_antecedentes = sum(
      n_antecedentes == 0
    ),
    
    con_antecedentes = sum(
      n_antecedentes > 0
    ),
    
    con_tres_antecedentes = sum(
      n_antecedentes == 3
    ),
    
    con_algun_cero = sum(
      antecedentes_cero > 0,
      na.rm = TRUE
    ),
    
    con_tres_positivos = sum(
      n_antecedentes == 3 &
        antecedentes_cero == 0,
      na.rm = TRUE
    ),
    
    factores_armonicos_cero = sum(
      factor_armonico_extendido == 0,
      na.rm = TRUE
    ),
    
    factores_armonicos_positivos = sum(
      factor_armonico_extendido > 0,
      na.rm = TRUE
    ),
    
    factores_armonicos_no_estimados = sum(
      is.na(factor_armonico_extendido)
    )
  )

print(resumen_alternativas_global)


# ------------------------------------------------------------
# 17.2.5. VALIDACIONES
# ------------------------------------------------------------

stopifnot(
  
  # Se conserva el universo completo de objetivos futuros.
  nrow(comparacion_factores_futuros) ==
    nrow(diagnostico_futuros_completo),
  
  # No se generan factores negativos.
  all(
    comparacion_factores_futuros$factor_armonico_extendido >= 0,
    na.rm = TRUE
  ),
  
  all(
    comparacion_factores_futuros$factor_aritmetico >= 0,
    na.rm = TRUE
  ),
  
  # Las combinaciones sin antecedentes no reciben factores.
  all(
    is.na(
      comparacion_factores_futuros$factor_armonico_extendido[
        comparacion_factores_futuros$n_antecedentes == 0
      ]
    )
  ),
  
  # Los factores armónicos de combinaciones con ceros
  # no pueden ser positivos bajo esta extensión.
  all(
    comparacion_factores_futuros$factor_armonico_extendido[
      comparacion_factores_futuros$antecedentes_cero > 0
    ] == 0,
    na.rm = TRUE
  )
)


# ------------------------------------------------------------
# 17.2.6. MOSTRAR RESULTADOS
# ------------------------------------------------------------

cat("\nRESUMEN GLOBAL DE ALTERNATIVAS:\n")

print(
  resumen_alternativas_global,
  width = Inf
)

cat("\nCOMPARACIÓN POR REZAGO Y CLASIFICACIÓN:\n")

print(
  resumen_alternativas_rezago,
  n = Inf,
  width = Inf
)

cat(
  "\nSECCIÓN 17.2 COMPLETADA.\n"
)


# ------------------------------------------------------------
# 17.3. ESTRUCTURA DE LOS FACTORES OBSERVADOS
# ------------------------------------------------------------

names(factores_registro_observados)

dplyr::glimpse(factores_registro_observados)



names(diagnostico_futuros_completo)

dplyr::glimpse(diagnostico_futuros_completo)



# ------------------------------------------------------------
# 17.3.1 AUDITORÍA DE CORRESPONDENCIA DE LOS ANTECEDENTES
# ------------------------------------------------------------

# 1. Expandir los antecedentes identificados en el diagnóstico.

antecedentes_expandido <-
  diagnostico_futuros_completo %>%
  dplyr::select(
    sexo,
    provincia_id,
    edad_quinquenal,
    anio_defuncion_objetivo,
    retraso_declaracion,
    n_antecedentes,
    anios_antecedentes,
    universos_antecedentes
  ) %>%
  tidyr::separate_rows(
    anios_antecedentes,
    universos_antecedentes,
    sep = ",\\s*"
  ) %>%
  dplyr::filter(
    !is.na(anios_antecedentes),
    !is.na(universos_antecedentes)
  ) %>%
  dplyr::mutate(
    anio_antecedente = as.integer(anios_antecedentes),
    universo_antecedente = as.integer(universos_antecedentes)
  ) %>%
  dplyr::select(
    -anios_antecedentes,
    -universos_antecedentes
  )


# 2. Recuperar las razones originales correspondientes.

auditoria_antecedentes <-
  antecedentes_expandido %>%
  dplyr::left_join(
    factores_registro_observados %>%
      dplyr::select(
        sexo,
        provincia_id,
        edad_quinquenal,
        anio_defuncion,
        retraso_declaracion,
        universo,
        razon_observada
      ),
    by = c(
      "sexo",
      "provincia_id",
      "edad_quinquenal",
      "anio_antecedente" = "anio_defuncion",
      "retraso_declaracion",
      "universo_antecedente" = "universo"
    )
  ) %>%
  dplyr::mutate(
    correspondencia_universo =
      universo_antecedente ==
      anio_antecedente + retraso_declaracion,
    
    razon_recuperada = !is.na(razon_observada)
  )


# 3. Resumen de la auditoría.

resumen_auditoria_antecedentes <-
  auditoria_antecedentes %>%
  dplyr::summarise(
    antecedentes_evaluados = dplyr::n(),
    
    razones_recuperadas =
      sum(razon_recuperada),
    
    razones_no_recuperadas =
      sum(!razon_recuperada),
    
    universos_correctos =
      sum(correspondencia_universo),
    
    universos_incorrectos =
      sum(!correspondencia_universo),
    
    razones_cero =
      sum(razon_observada == 0, na.rm = TRUE),
    
    razones_positivas =
      sum(razon_observada > 0, na.rm = TRUE)
  )

print(resumen_auditoria_antecedentes)


# 4. Identificar posibles inconsistencias.

inconsistencias_antecedentes <-
  auditoria_antecedentes %>%
  dplyr::filter(
    !razon_recuperada |
      !correspondencia_universo
  )

print(
  dplyr::glimpse(inconsistencias_antecedentes)
)


print(resumen_auditoria_antecedentes, width = Inf)


# ------------------------------------------------------------
# 17.3.2. DISTRIBUCIÓN DE ANTECEDENTES HISTÓRICOS ÚNICOS
# ------------------------------------------------------------

# 1. Identificar cada factor histórico una sola vez.

antecedentes_unicos <-
  auditoria_antecedentes %>%
  dplyr::distinct(
    sexo,
    provincia_id,
    edad_quinquenal,
    anio_antecedente,
    retraso_declaracion,
    universo_antecedente,
    razon_observada
  )


# 2. Resumir por rezago de registro.

resumen_antecedentes_unicos <-
  antecedentes_unicos %>%
  dplyr::group_by(retraso_declaracion) %>%
  dplyr::summarise(
    antecedentes_unicos = dplyr::n(),
    
    razones_cero =
      sum(razon_observada == 0, na.rm = TRUE),
    
    razones_positivas =
      sum(razon_observada > 0, na.rm = TRUE),
    
    pct_cero =
      100 * razones_cero / antecedentes_unicos,
    
    pct_positivas =
      100 * razones_positivas / antecedentes_unicos,
    
    .groups = "drop"
  )

print(resumen_antecedentes_unicos, n = Inf)


# ------------------------------------------------------------
# 17.3.3. VALIDACIÓN DE UNICIDAD DE LOS FACTORES HISTÓRICOS
# ------------------------------------------------------------

auditoria_unicidad_factores <-
  auditoria_antecedentes %>%
  dplyr::group_by(
    sexo,
    provincia_id,
    edad_quinquenal,
    anio_antecedente,
    retraso_declaracion
  ) %>%
  dplyr::summarise(
    n_registros = dplyr::n(),
    n_razones = dplyr::n_distinct(razon_observada),
    razon_minima = min(razon_observada, na.rm = TRUE),
    razon_maxima = max(razon_observada, na.rm = TRUE),
    .groups = "drop"
  )

resumen_unicidad_factores <-
  auditoria_unicidad_factores %>%
  dplyr::summarise(
    combinaciones_historicas = dplyr::n(),
    combinaciones_con_una_razon =
      sum(n_razones == 1),
    combinaciones_con_varias_razones =
      sum(n_razones > 1),
    maximo_registros_por_combinacion =
      max(n_registros)
  )

print(resumen_unicidad_factores, width = Inf)

inconsistencias_unicidad <-
  auditoria_unicidad_factores %>%
  dplyr::filter(n_razones > 1)

print(dplyr::glimpse(inconsistencias_unicidad))



# ------------------------------------------------------------
# 17.3.4. CLASIFICACIÓN METODOLÓGICA DE ANTECEDENTES
# ------------------------------------------------------------

clasificacion_antecedentes <-
  auditoria_antecedentes %>%
  dplyr::group_by(
    sexo,
    provincia_id,
    edad_quinquenal,
    anio_defuncion_objetivo,
    retraso_declaracion
  ) %>%
  dplyr::summarise(
    n_antecedentes = dplyr::n(),
    
    n_positivos =
      sum(razon_observada > 0),
    
    n_ceros =
      sum(razon_observada == 0),
    
    n_disponibles =
      sum(!is.na(razon_observada)),
    
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    clasificacion = dplyr::case_when(
      n_disponibles == 3 & n_positivos == 3 ~
        "Tres antecedentes positivos",
      
      n_disponibles == 3 & n_positivos == 2 ~
        "Dos positivos y un cero",
      
      n_disponibles == 3 & n_positivos == 1 ~
        "Un positivo y dos ceros",
      
      n_disponibles == 3 & n_ceros == 3 ~
        "Tres antecedentes cero",
      
      n_disponibles == 2 & n_positivos == 2 ~
        "Dos antecedentes positivos",
      
      n_disponibles == 2 & n_positivos == 1 ~
        "Un positivo y un cero",
      
      n_disponibles == 2 & n_ceros == 2 ~
        "Dos antecedentes cero",
      
      n_disponibles == 1 & n_positivos == 1 ~
        "Un antecedente positivo",
      
      n_disponibles == 1 & n_ceros == 1 ~
        "Un antecedente cero",
      
      TRUE ~ "Sin antecedentes disponibles"
    )
  )


# Resumen de las combinaciones por clasificación.

resumen_clasificacion_antecedentes <-
  clasificacion_antecedentes %>%
  dplyr::count(
    retraso_declaracion,
    clasificacion,
    name = "combinaciones"
  ) %>%
  dplyr::group_by(retraso_declaracion) %>%
  dplyr::mutate(
    porcentaje =
      100 * combinaciones / sum(combinaciones)
  ) %>%
  dplyr::ungroup()

print(resumen_clasificacion_antecedentes, n = Inf)


# ------------------------------------------------------------
# 17.3.5. CLASIFICACIÓN COMPLETA DE ANTECEDENTES FUTUROS
# ------------------------------------------------------------

clasificacion_completa_antecedentes <-
  diagnostico_futuros_completo %>%
  dplyr::mutate(
    clasificacion = dplyr::case_when(
      
      n_antecedentes == 0 ~
        "Sin antecedentes",
      
      n_antecedentes == 3 & antecedentes_positivos == 3 ~
        "Tres antecedentes positivos",
      
      n_antecedentes == 3 & antecedentes_positivos == 2 ~
        "Dos positivos y un cero",
      
      n_antecedentes == 3 & antecedentes_positivos == 1 ~
        "Un positivo y dos ceros",
      
      n_antecedentes == 3 & antecedentes_cero == 3 ~
        "Tres antecedentes cero",
      
      n_antecedentes == 2 & antecedentes_positivos == 2 ~
        "Dos antecedentes positivos",
      
      n_antecedentes == 2 & antecedentes_positivos == 1 ~
        "Un positivo y un cero",
      
      n_antecedentes == 2 & antecedentes_cero == 2 ~
        "Dos antecedentes cero",
      
      n_antecedentes == 1 & antecedentes_positivos == 1 ~
        "Un antecedente positivo",
      
      n_antecedentes == 1 & antecedentes_cero == 1 ~
        "Un antecedente cero",
      
      TRUE ~ "Revisar clasificación"
    )
  )


# Resumen por rezago.

resumen_clasificacion_completa <-
  clasificacion_completa_antecedentes %>%
  dplyr::count(
    retraso_declaracion,
    clasificacion,
    name = "combinaciones"
  ) %>%
  dplyr::group_by(retraso_declaracion) %>%
  dplyr::mutate(
    porcentaje =
      100 * combinaciones / sum(combinaciones)
  ) %>%
  dplyr::ungroup()


print(resumen_clasificacion_completa, n = Inf)


# ------------------------------------------------------------
# 17.3.6. COMPARACIÓN DE ESCENARIOS DE ESTIMACIÓN
# ------------------------------------------------------------

escenarios_estimacion <-
  auditoria_antecedentes %>%
  dplyr::group_by(
    sexo,
    provincia_id,
    edad_quinquenal,
    anio_defuncion_objetivo,
    retraso_declaracion
  ) %>%
  dplyr::summarise(
    
    n_antecedentes = dplyr::n(),
    
    n_ceros =
      sum(razon_observada == 0),
    
    n_positivos =
      sum(razon_observada > 0),
    
    # Escenario 1: media armónica de razones positivas.
    factor_armonico_positivos = {
      x <- razon_observada[razon_observada > 0]
      
      if (length(x) > 0) {
        length(x) / sum(1 / x)
      } else {
        NA_real_
      }
    },
    
    # Escenario 2: media aritmética, incluyendo ceros.
    factor_aritmetico =
      mean(razon_observada),
    
    # Escenario 3: regla conservadora.
    factor_conservador = {
      if (any(razon_observada == 0)) {
        0
      } else {
        length(razon_observada) /
          sum(1 / razon_observada)
      }
    },
    
    .groups = "drop"
  )

dim(escenarios_estimacion)

dplyr::count(
  escenarios_estimacion,
  retraso_declaracion,
  n_antecedentes,
  n_ceros,
  n_positivos
)


# ------------------------------------------------------------
# 17.3.7. INTEGRACIÓN DE ESCENARIOS Y CASOS SIN ANTECEDENTES
# ------------------------------------------------------------

escenarios_estimacion_completos <-
  diagnostico_futuros_completo %>%
  dplyr::select(
    sexo,
    provincia_id,
    edad_quinquenal,
    anio_defuncion_objetivo,
    retraso_declaracion,
    n_antecedentes,
    antecedentes_positivos,
    antecedentes_cero,
    clasificacion_antecedentes_futura
  ) %>%
  dplyr::left_join(
    escenarios_estimacion %>%
      dplyr::select(
        sexo,
        provincia_id,
        edad_quinquenal,
        anio_defuncion_objetivo,
        retraso_declaracion,
        factor_armonico_positivos,
        factor_aritmetico,
        factor_conservador
      ),
    by = c(
      "sexo",
      "provincia_id",
      "edad_quinquenal",
      "anio_defuncion_objetivo",
      "retraso_declaracion"
    )
  )


# Comprobar integración de los escenarios.

escenarios_estimacion_completos %>%
  dplyr::summarise(
    combinaciones = dplyr::n(),
    sin_antecedentes =
      sum(n_antecedentes == 0),
    con_antecedentes =
      sum(n_antecedentes > 0),
    sin_factor_armonico =
      sum(is.na(factor_armonico_positivos)),
    sin_factor_aritmetico =
      sum(is.na(factor_aritmetico)),
    sin_factor_conservador =
      sum(is.na(factor_conservador))
  )


escenarios_estimacion_completos %>%
  dplyr::summarise(
    sin_factor_armonico =
      sum(is.na(factor_armonico_positivos)),
    sin_factor_aritmetico =
      sum(is.na(factor_aritmetico)),
    sin_factor_conservador =
      sum(is.na(factor_conservador)),
    armonico_disponible =
      sum(!is.na(factor_armonico_positivos)),
    aritmetico_disponible =
      sum(!is.na(factor_aritmetico)),
    conservador_disponible =
      sum(!is.na(factor_conservador))
  )


# ------------------------------------------------------------
# 17.3.8. COMPARACIÓN DESCRIPTIVA DE FACTORES ESTIMADOS
# ------------------------------------------------------------

resumen_escenarios <-
  escenarios_estimacion_completos %>%
  dplyr::summarise(
    dplyr::across(
      c(
        factor_armonico_positivos,
        factor_aritmetico,
        factor_conservador
      ),
      list(
        media = ~ mean(.x, na.rm = TRUE),
        mediana = ~ median(.x, na.rm = TRUE),
        minimo = ~ min(.x, na.rm = TRUE),
        maximo = ~ max(.x, na.rm = TRUE)
      )
    )
  )

resumen_escenarios

# Mostrar todos los estadísticos sin abreviación.
print(resumen_escenarios, width = Inf)


# Comparación sobre las mismas combinaciones:
# aquellas con al menos un antecedente positivo.

comparacion_mismos_casos <-
  escenarios_estimacion_completos %>%
  dplyr::filter(
    antecedentes_positivos > 0
  ) %>%
  dplyr::summarise(
    n = dplyr::n(),
    
    media_armonica =
      mean(factor_armonico_positivos, na.rm = TRUE),
    
    media_aritmetica =
      mean(factor_aritmetico, na.rm = TRUE),
    
    media_conservadora =
      mean(factor_conservador, na.rm = TRUE),
    
    mediana_armonica =
      median(factor_armonico_positivos, na.rm = TRUE),
    
    mediana_aritmetica =
      median(factor_aritmetico, na.rm = TRUE),
    
    mediana_conservadora =
      median(factor_conservador, na.rm = TRUE)
  )

comparacion_mismos_casos



#------------------------------------------------------------
# 17.3.9. DISTRIBUCIÓN Y VALORES EXTREMOS
# ------------------------------------------------------------

# Estadísticos completos sobre las mismas combinaciones.

comparacion_mismos_casos_completa <-
  escenarios_estimacion_completos %>%
  dplyr::filter(antecedentes_positivos > 0) %>%
  dplyr::summarise(
    n = dplyr::n(),
    
    media_armonica =
      mean(factor_armonico_positivos),
    
    media_aritmetica =
      mean(factor_aritmetico),
    
    media_conservadora =
      mean(factor_conservador),
    
    mediana_armonica =
      median(factor_armonico_positivos),
    
    mediana_aritmetica =
      median(factor_aritmetico),
    
    mediana_conservadora =
      median(factor_conservador)
  )

comparacion_mismos_casos_completa

# Identificar los 10 factores armónicos más elevados.

escenarios_estimacion_completos %>%
  dplyr::filter(antecedentes_positivos > 0) %>%
  dplyr::summarise(
    n = dplyr::n(),
    media_armonica = mean(factor_armonico_positivos),
    media_aritmetica = mean(factor_aritmetico),
    media_conservadora = mean(factor_conservador),
    mediana_armonica = median(factor_armonico_positivos),
    mediana_aritmetica = median(factor_aritmetico),
    mediana_conservadora = median(factor_conservador)
  )

escenarios_estimacion_completos %>%
  dplyr::filter(antecedentes_positivos > 0) %>%
  dplyr::summarise(
    mediana_aritmetica =
      median(factor_aritmetico),
    mediana_conservadora =
      median(factor_conservador)
  ) %>%
  print(width = Inf)

# ------------------------------------------------------------
# 17.3.10. IDENTIFICACIÓN DE FACTORES ARMÓNICOS EXTREMOS
# ------------------------------------------------------------

escenarios_estimacion_completos %>%
  dplyr::filter(
    !is.na(factor_armonico_positivos)
  ) %>%
  dplyr::left_join(
    escenarios_estimacion %>%
      dplyr::select(
        sexo,
        provincia_id,
        edad_quinquenal,
        anio_defuncion_objetivo,
        retraso_declaracion,
        n_ceros,
        n_positivos
      ),
    by = c(
      "sexo",
      "provincia_id",
      "edad_quinquenal",
      "anio_defuncion_objetivo",
      "retraso_declaracion"
    )
  ) %>%
  dplyr::arrange(
    dplyr::desc(factor_armonico_positivos)
  ) %>%
  dplyr::select(
    sexo,
    provincia_id,
    edad_quinquenal,
    anio_defuncion_objetivo,
    retraso_declaracion,
    n_antecedentes,
    n_ceros,
    n_positivos,
    factor_armonico_positivos,
    factor_aritmetico,
    factor_conservador
  ) %>%
  dplyr::slice_head(n = 10) %>%
  print(n = 10, width = Inf)


escenarios_estimacion_completos %>%
  dplyr::filter(
    !is.na(factor_armonico_positivos)
  ) %>%
  dplyr::left_join(
    escenarios_estimacion %>%
      dplyr::select(
        sexo,
        provincia_id,
        edad_quinquenal,
        anio_defuncion_objetivo,
        retraso_declaracion,
        n_ceros,
        n_positivos
      ),
    by = c(
      "sexo",
      "provincia_id",
      "edad_quinquenal",
      "anio_defuncion_objetivo",
      "retraso_declaracion"
    )
  ) %>%
  dplyr::count(
    n_antecedentes,
    n_positivos,
    n_ceros,
    name = "combinaciones"
  ) %>%
  dplyr::arrange(
    n_positivos,
    n_antecedentes,
    n_ceros
  )


# ------------------------------------------------------------
# 17.3.11. DISTRIBUCIÓN DE FACTORES ARITMÉTICOS POR REZAGO
# ------------------------------------------------------------

resumen_aritmetico_rezago <-
  escenarios_estimacion_completos %>%
  dplyr::filter(
    !is.na(factor_aritmetico)
  ) %>%
  dplyr::group_by(
    retraso_declaracion
  ) %>%
  dplyr::summarise(
    combinaciones = dplyr::n(),
    
    factores_cero =
      sum(factor_aritmetico == 0),
    
    pct_factores_cero =
      100 * mean(factor_aritmetico == 0),
    
    media =
      mean(factor_aritmetico),
    
    mediana =
      median(factor_aritmetico),
    
    minimo =
      min(factor_aritmetico),
    
    maximo =
      max(factor_aritmetico),
    
    .groups = "drop"
  )

resumen_aritmetico_rezago


# ------------------------------------------------------------
# 17.3.12. FACTORES ARITMÉTICOS POSITIVOS POR REZAGO
# ------------------------------------------------------------

resumen_aritmetico_positivo_rezago <-
  escenarios_estimacion_completos %>%
  dplyr::filter(
    !is.na(factor_aritmetico),
    factor_aritmetico > 0
  ) %>%
  dplyr::group_by(
    retraso_declaracion
  ) %>%
  dplyr::summarise(
    combinaciones_positivas = dplyr::n(),
    
    media =
      mean(factor_aritmetico),
    
    mediana =
      median(factor_aritmetico),
    
    minimo =
      min(factor_aritmetico),
    
    maximo =
      max(factor_aritmetico),
    
    .groups = "drop"
  )

resumen_aritmetico_positivo_rezago


escenarios_estimacion_completos %>%
  dplyr::filter(
    !is.na(factor_aritmetico),
    factor_aritmetico > 0
  ) %>%
  dplyr::group_by(
    retraso_declaracion
  ) %>%
  dplyr::summarise(
    combinaciones_positivas = dplyr::n(),
    media = mean(factor_aritmetico),
    mediana = median(factor_aritmetico),
    minimo = min(factor_aritmetico),
    maximo = max(factor_aritmetico),
    .groups = "drop"
  )


# ------------------------------------------------------------
# 17.3.13. COMPOSICIÓN DE FACTORES ARITMÉTICOS POSITIVOS
# ------------------------------------------------------------

escenarios_estimacion_completos %>%
  dplyr::filter(
    !is.na(factor_aritmetico),
    factor_aritmetico > 0
  ) %>%
  dplyr::count(
    retraso_declaracion,
    n_antecedentes,
    antecedentes_positivos,
    antecedentes_cero,
    name = "combinaciones"
  ) %>%
  dplyr::arrange(
    retraso_declaracion,
    antecedentes_positivos,
    antecedentes_cero
  )

escenarios_estimacion_completos %>%
  dplyr::filter(
    !is.na(factor_aritmetico),
    factor_aritmetico > 0
  ) %>%
  dplyr::count(
    retraso_declaracion,
    n_antecedentes,
    antecedentes_positivos,
    antecedentes_cero,
    name = "combinaciones"
  ) %>%
  dplyr::arrange(
    retraso_declaracion,
    antecedentes_positivos,
    antecedentes_cero
  ) %>%
  print(n = Inf, width = Inf)


# ============================================================
# 17.4. ESTIMACIÓN DE FACTORES Y DEFUNCIONES TARDÍAS FALTANTES
#      ADAPTACIÓN ESTRICTA DE LA MEDIA ARMÓNICA
# ============================================================

# Se aplica la media armónica únicamente cuando los tres
# antecedentes seleccionados son positivos.
#
# Las combinaciones con ceros o menos de tres antecedentes
# permanecen sin estimación.
#
# La estimación de defunciones tardías se obtiene mediante:
#
# Defunciones tardías estimadas =
# Factor estimado × Defunciones oportunas


# ------------------------------------------------------------
# 17.4.1. ESTIMAR LOS FACTORES DE REGISTRO TARDÍO
# ------------------------------------------------------------

factores_estimados_inec <-
  tres_antecedentes_armonico %>%
  dplyr::group_by(
    sexo,
    provincia_id,
    provincia,
    edad_quinquenal,
    anio_defuncion_objetivo,
    retraso_declaracion
  ) %>%
  dplyr::summarise(
    
    n_antecedentes = dplyr::n(),
    
    antecedentes_positivos = sum(
      razon_antecedente > 0
    ),
    
    antecedentes_cero = sum(
      razon_antecedente == 0
    ),
    
    factor_estimado = {
      x <- razon_antecedente
      
      if (
        length(x) == 3L &&
        all(x > 0)
      ) {
        length(x) / sum(1 / x)
      } else {
        NA_real_
      }
    },
    
    .groups = "drop"
  )


# ------------------------------------------------------------
# 17.4.2. INTEGRAR TODAS LAS COMBINACIONES OBJETIVO
# ------------------------------------------------------------

diagnostico_estimacion_inec <-
  diagnostico_futuros_completo %>%
  dplyr::select(
    sexo,
    provincia_id,
    provincia,
    edad_quinquenal,
    anio_defuncion_objetivo,
    retraso_declaracion,
    universo_objetivo,
    n_antecedentes,
    antecedentes_positivos,
    antecedentes_cero
  ) %>%
  dplyr::left_join(
    factores_estimados_inec %>%
      dplyr::select(
        sexo,
        provincia_id,
        edad_quinquenal,
        anio_defuncion_objetivo,
        retraso_declaracion,
        factor_estimado
      ),
    by = c(
      "sexo",
      "provincia_id",
      "edad_quinquenal",
      "anio_defuncion_objetivo",
      "retraso_declaracion"
    )
  )


# ------------------------------------------------------------
# 17.4.3. INCORPORAR LAS DEFUNCIONES OPORTUNAS
# ------------------------------------------------------------

diagnostico_estimacion_inec <-
  diagnostico_estimacion_inec %>%
  dplyr::left_join(
    defunciones_oportunas_estratificadas %>%
      dplyr::select(
        anio_defuncion,
        sexo,
        provincia_id,
        edad_quinquenal,
        defunciones_oportunas
      ),
    by = c(
      "anio_defuncion_objetivo" = "anio_defuncion",
      "sexo",
      "provincia_id",
      "edad_quinquenal"
    )
  )


# ------------------------------------------------------------
# 17.4.4. CALCULAR LAS DEFUNCIONES TARDÍAS ESTIMADAS
# ------------------------------------------------------------

diagnostico_estimacion_inec <-
  diagnostico_estimacion_inec %>%
  dplyr::mutate(
    defunciones_tardias_estimadas = dplyr::if_else(
      !is.na(factor_estimado) &
        factor_estimado > 0 &
        !is.na(defunciones_oportunas) &
        defunciones_oportunas > 0,
      
      factor_estimado * defunciones_oportunas,
      
      NA_real_
    )
  )


# ------------------------------------------------------------
# 17.4.5. CONSERVAR ÚNICAMENTE LAS CORRECCIONES POSITIVAS
# ------------------------------------------------------------

correcciones_tardias_estimadas <-
  diagnostico_estimacion_inec %>%
  dplyr::filter(
    !is.na(factor_estimado),
    factor_estimado > 0,
    !is.na(defunciones_tardias_estimadas),
    defunciones_tardias_estimadas > 0
  ) %>%
  dplyr::arrange(
    anio_defuncion_objetivo,
    sexo,
    provincia_id,
    edad_quinquenal,
    retraso_declaracion
  )


# ------------------------------------------------------------
# 17.4.6. RESUMEN DE LA ESTIMACIÓN
# ------------------------------------------------------------

resumen_estimacion_inec <-
  diagnostico_estimacion_inec %>%
  dplyr::summarise(
    
    combinaciones_objetivo = dplyr::n(),
    
    tres_antecedentes_positivos = sum(
      n_antecedentes == 3 &
        antecedentes_positivos == 3
    ),
    
    combinaciones_con_ceros = sum(
      antecedentes_cero > 0
    ),
    
    antecedentes_insuficientes = sum(
      n_antecedentes < 3
    ),
    
    factores_estimados = sum(
      !is.na(factor_estimado)
    ),
    
    correcciones_calculadas = sum(
      !is.na(defunciones_tardias_estimadas)
    ),
    
    total_defunciones_tardias_estimadas = sum(
      defunciones_tardias_estimadas,
      na.rm = TRUE
    )
  )

print(resumen_estimacion_inec, width = Inf)


# ------------------------------------------------------------
# 17.4.7. VALIDACIONES
# ------------------------------------------------------------

stopifnot(
  
  # Se conserva la totalidad de combinaciones objetivo.
  nrow(diagnostico_estimacion_inec) ==
    nrow(diagnostico_futuros_completo),
  
  # Todos los factores estimados tienen tres antecedentes.
  all(
    diagnostico_estimacion_inec$n_antecedentes[
      !is.na(diagnostico_estimacion_inec$factor_estimado)
    ] == 3
  ),
  
  # Todos los factores estimados tienen tres antecedentes positivos.
  all(
    diagnostico_estimacion_inec$antecedentes_positivos[
      !is.na(diagnostico_estimacion_inec$factor_estimado)
    ] == 3
  ),
  
  # No se estiman factores iguales o inferiores a cero.
  all(
    diagnostico_estimacion_inec$factor_estimado[
      !is.na(diagnostico_estimacion_inec$factor_estimado)
    ] > 0
  ),
  
  # No se calculan correcciones negativas.
  all(
    correcciones_tardias_estimadas$defunciones_tardias_estimadas > 0
  )
)


cat("\nSECCIÓN 17.4 COMPLETADA.\n")



# ============================================================
# 17.5. CONSOLIDAR DEFUNCIONES TARDÍAS OBSERVADAS
#      POR AÑO DE OCURRENCIA, SEXO, PROVINCIA Y EDAD
# ============================================================

defunciones_tardias_observadas <-
  matriz_estratificada_inec %>%
  dplyr::filter(
    anio_defuncion %in% 2015:2025,
    universo <= 2025,
    retraso_declaracion %in% 1:9,
    categoria_disponible
  ) %>%
  dplyr::group_by(
    anio_defuncion,
    sexo,
    provincia_id,
    provincia,
    edad_quinquenal
  ) %>%
  dplyr::summarise(
    defunciones_tardias_observadas = sum(
      defunciones,
      na.rm = TRUE
    ),
    rezagos_observados = dplyr::n(),
    .groups = "drop"
  ) %>%
  dplyr::arrange(
    anio_defuncion,
    sexo,
    provincia_id,
    edad_quinquenal
  )


# ------------------------------------------------------------
# VALIDAR LA TABLA DE DEFUNCIONES TARDÍAS OBSERVADAS
# ------------------------------------------------------------

stopifnot(
  !anyDuplicated(
    defunciones_tardias_observadas %>%
      dplyr::select(
        anio_defuncion,
        sexo,
        provincia_id,
        edad_quinquenal
      )
  ),
  all(
    defunciones_tardias_observadas$
      defunciones_tardias_observadas >= 0
  ),
  all(
    defunciones_tardias_observadas$
      rezagos_observados >= 1
  )
)

cat("\nDEFUNCIONES TARDÍAS OBSERVADAS\n")

print(
  defunciones_tardias_observadas %>%
    dplyr::summarise(
      combinaciones = dplyr::n(),
      total_observado = sum(
        defunciones_tardias_observadas
      )
    )
)

# ------------------------------------------------------------
# INSPECCIONAR LA ESTRUCTURA DE LAS ESTIMACIONES
# ------------------------------------------------------------

cat("\nCOLUMNAS DE correcciones_tardias_estimadas:\n")

print(
  names(correcciones_tardias_estimadas)
)

cat("\nESTRUCTURA DE correcciones_tardias_estimadas:\n")

dplyr::glimpse(correcciones_tardias_estimadas)



# ============================================================
# 17.6. INTEGRAR DEFUNCIONES TARDÍAS OBSERVADAS Y ESTIMADAS
#      POR AÑO DE OCURRENCIA, SEXO, PROVINCIA Y EDAD
# ============================================================

# 1. Preparar las estimaciones con la misma estructura
#    de identificación de las defunciones observadas.
#    Se suman los rezagos para evitar claves duplicadas.

defunciones_tardias_estimadas <-
  correcciones_tardias_estimadas %>%
  dplyr::group_by(
    anio_defuncion = anio_defuncion_objetivo,
    sexo,
    provincia_id,
    provincia,
    edad_quinquenal
  ) %>%
  dplyr::summarise(
    defunciones_tardias_estimadas = sum(defunciones_tardias_estimadas, na.rm = TRUE),
    .groups = "drop"
  )

# 2. Validar que no existan claves duplicadas
#    en ninguna de las dos fuentes.

stopifnot(
  !anyDuplicated(
    defunciones_tardias_observadas %>%
      dplyr::select(
        anio_defuncion,
        sexo,
        provincia_id,
        edad_quinquenal
      )
  ),
  !anyDuplicated(
    defunciones_tardias_estimadas %>%
      dplyr::select(
        anio_defuncion,
        sexo,
        provincia_id,
        edad_quinquenal
      )
  )
)


# 3. Integrar ambas fuentes mediante unión completa.
#    Las combinaciones sin valor en una fuente reciben cero.

defunciones_tardias_integradas <-
  dplyr::full_join(
    defunciones_tardias_observadas %>%
      dplyr::select(
        anio_defuncion,
        sexo,
        provincia_id,
        provincia,
        edad_quinquenal,
        defunciones_tardias_observadas
      ),
    defunciones_tardias_estimadas,
    by = c(
      "anio_defuncion",
      "sexo",
      "provincia_id",
      "provincia",
      "edad_quinquenal"
    )
  ) %>%
  dplyr::mutate(
    dplyr::across(
      c(
        defunciones_tardias_observadas,
        defunciones_tardias_estimadas
      ),
      ~ tidyr::replace_na(.x, 0)
    ),
    defunciones_tardias_total =
      defunciones_tardias_observadas +
      defunciones_tardias_estimadas
  ) %>%
  dplyr::arrange(
    anio_defuncion,
    sexo,
    provincia_id,
    edad_quinquenal
  )


# 4. Validaciones de la integración.

stopifnot(
  !anyDuplicated(
    defunciones_tardias_integradas %>%
      dplyr::select(
        anio_defuncion,
        sexo,
        provincia_id,
        edad_quinquenal
      )
  ),
  all(
    defunciones_tardias_integradas$defunciones_tardias_observadas >= 0
  ),
  all(
    defunciones_tardias_integradas$defunciones_tardias_estimadas >= 0
  ),
  all(
    defunciones_tardias_integradas$defunciones_tardias_total >= 0
  ),
  isTRUE(all.equal(
    sum(defunciones_tardias_integradas$defunciones_tardias_observadas),
    1545
  )),
  isTRUE(all.equal(
    sum(defunciones_tardias_integradas$defunciones_tardias_estimadas),
    sum(correcciones_tardias_estimadas$defunciones_tardias_estimadas)
  ))
)


# 5. Resumen de la integración.

cat("\nDEFUNCIONES TARDÍAS INTEGRADAS\n")

print(
  defunciones_tardias_integradas %>%
    dplyr::summarise(
      combinaciones = dplyr::n(),
      tardias_observadas = sum(
        defunciones_tardias_observadas
      ),
      tardias_estimadas = sum(
        defunciones_tardias_estimadas
      ),
      tardias_totales = sum(
        defunciones_tardias_total
      )
    )
)


# 6. Comprobar posibles coincidencias de claves
#    entre las fuentes observadas y estimadas.

cat("\nCOINCIDENCIAS ENTRE FUENTES\n")

print(
  dplyr::inner_join(
    defunciones_tardias_observadas %>%
      dplyr::select(
        anio_defuncion,
        sexo,
        provincia_id,
        edad_quinquenal
      ),
    defunciones_tardias_estimadas %>%
      dplyr::select(
        anio_defuncion,
        sexo,
        provincia_id,
        edad_quinquenal
      ),
    by = c(
      "anio_defuncion",
      "sexo",
      "provincia_id",
      "edad_quinquenal"
    )
  ) %>%
    dplyr::summarise(
      claves_compartidas = dplyr::n()
    )
)


cat("\nSECCIÓN 17.6 COMPLETADA.\n")




# ============================================================
# 17.7. AUDITORÍA DE COMPATIBILIDAD CON LA BASE DE MORTALIDAD
# ============================================================

# 1. Inspeccionar las estructuras.

cat("\nESTRUCTURA DE base_mortalidad\n")
dplyr::glimpse(base_mortalidad)

cat("\nESTRUCTURA DE defunciones_tardias_integradas\n")
dplyr::glimpse(defunciones_tardias_integradas)


# 2. Comparar las categorías de sexo.

cat("\nCATEGORÍAS DE SEXO\n")

sexos_base <-
  sort(unique(as.character(base_mortalidad$sexo)))

sexos_tardias <-
  sort(unique(as.character(defunciones_tardias_integradas$sexo)))

print(list(
  base_mortalidad = sexos_base,
  tardias_integradas = sexos_tardias,
  exclusivos_base = setdiff(sexos_base, sexos_tardias),
  exclusivos_tardias = setdiff(sexos_tardias, sexos_base)
))


# 3. Comparar las categorías territoriales.
#    Se contrasta region con provincia, sin asumir todavía
#    que ambas variables sean equivalentes.

cat("\nCATEGORÍAS TERRITORIALES\n")

regiones_base <-
  sort(unique(as.character(base_mortalidad$region)))

provincias_tardias <-
  sort(unique(as.character(defunciones_tardias_integradas$provincia)))

print(list(
  regiones_base = regiones_base,
  provincias_tardias = provincias_tardias,
  exclusivos_base = setdiff(regiones_base, provincias_tardias),
  exclusivos_tardias = setdiff(provincias_tardias, regiones_base)
))


# 4. Comparar las categorías de edad.

cat("\nCATEGORÍAS DE EDAD\n")

edades_base <-
  sort(unique(as.character(base_mortalidad$edad)))

edades_tardias <-
  sort(unique(as.character(defunciones_tardias_integradas$edad_quinquenal)))

print(list(
  edades_base = edades_base,
  edades_tardias = edades_tardias,
  exclusivos_base = setdiff(edades_base, edades_tardias),
  exclusivos_tardias = setdiff(edades_tardias, edades_base)
))


# 5. Inspeccionar los años disponibles.
#    No se interpretan como años equivalentes:
#    base_mortalidad$anio = universo REDATAM;
#    tardías_integradas$anio_defuncion = año de ocurrencia.

cat("\nAÑOS DISPONIBLES\n")

print(list(
  universos_base = sort(unique(base_mortalidad$anio)),
  ocurrencias_tardias = sort(unique(
    defunciones_tardias_integradas$anio_defuncion
  ))
))


# 6. Comprobar la unicidad de las combinaciones
#    en la base de mortalidad.

cat("\nUNICIDAD DE CLAVES EN base_mortalidad\n")

duplicados_base <-
  base_mortalidad %>%
  dplyr::count(
    anio,
    sexo,
    region,
    edad,
    name = "frecuencia"
  ) %>%
  dplyr::filter(frecuencia > 1)

print(
  dplyr::summarise(
    duplicados_base,
    combinaciones_duplicadas = dplyr::n()
  )
)


# 7. Comprobar la unicidad de las combinaciones
#    en la tabla de tardías integradas.

cat("\nUNICIDAD DE CLAVES EN tardías integradas\n")

duplicados_tardias <-
  defunciones_tardias_integradas %>%
  dplyr::count(
    anio_defuncion,
    sexo,
    provincia_id,
    edad_quinquenal,
    name = "frecuencia"
  ) %>%
  dplyr::filter(frecuencia > 1)

print(
  dplyr::summarise(
    duplicados_tardias,
    combinaciones_duplicadas = dplyr::n()
  )
)


# 8. Revisar el rango y los valores de las variables
#    de mortalidad y exposición.

cat("\nRESUMEN DE VARIABLES DE base_mortalidad\n")

print(
  base_mortalidad %>%
    dplyr::summarise(
      filas = dplyr::n(),
      exposicion_total = sum(exposicion, na.rm = TRUE),
      mortalidad_observada_total = sum(
        mortalidad_observada,
        na.rm = TRUE
      ),
      mortalidad_total = sum(
        mortalidad,
        na.rm = TRUE
      ),
      mx_minimo = min(mx, na.rm = TRUE),
      mx_maximo = max(mx, na.rm = TRUE),
      mx1000_minimo = min(mx_1000, na.rm = TRUE),
      mx1000_maximo = max(mx_1000, na.rm = TRUE)
    )
)


cat("\nSECCIÓN 17.7 COMPLETADA.\n")


# ============================================================
# 17.8. AUDITAR DEFUNCIONES TARDÍAS CON EDAD IGNORADA
# ============================================================

# 1. Cuantificar las defunciones tardías de edad ignorada.

auditoria_edad_ignorada_tardias <-
  defunciones_tardias_integradas %>%
  dplyr::filter(
    edad_quinquenal == "Edad ignorada"
  ) %>%
  dplyr::summarise(
    combinaciones = dplyr::n(),
    combinaciones_observadas_positivas = sum(
      defunciones_tardias_observadas > 0
    ),
    combinaciones_estimadas_positivas = sum(
      defunciones_tardias_estimadas > 0
    ),
    tardias_observadas = sum(
      defunciones_tardias_observadas
    ),
    tardias_estimadas = sum(
      defunciones_tardias_estimadas
    ),
    tardias_totales = sum(
      defunciones_tardias_total
    )
  )

cat("\nRESUMEN DE EDAD IGNORADA\n")
print(auditoria_edad_ignorada_tardias)


# 2. Distribución por año de ocurrencia y sexo.

cat("\nEDAD IGNORADA POR AÑO Y SEXO\n")

print(
  defunciones_tardias_integradas %>%
    dplyr::filter(
      edad_quinquenal == "Edad ignorada"
    ) %>%
    dplyr::group_by(
      anio_defuncion,
      sexo
    ) %>%
    dplyr::summarise(
      tardias_observadas = sum(
        defunciones_tardias_observadas
      ),
      tardias_estimadas = sum(
        defunciones_tardias_estimadas
      ),
      tardias_totales = sum(
        defunciones_tardias_total
      ),
      .groups = "drop"
    ) %>%
    dplyr::arrange(
      anio_defuncion,
      sexo
    ) %>%
    dplyr::filter(
      tardias_totales > 0
    )
)


# 3. Identificar las combinaciones territoriales
#    y demográficas con tardías observadas de edad ignorada.

cat("\nREGISTROS POSITIVOS DE EDAD IGNORADA\n")

print(
  defunciones_tardias_integradas %>%
    dplyr::filter(
      edad_quinquenal == "Edad ignorada",
      defunciones_tardias_observadas > 0
    ) %>%
    dplyr::select(
      anio_defuncion,
      sexo,
      provincia,
      defunciones_tardias_observadas
    ) %>%
    dplyr::arrange(
      anio_defuncion,
      sexo,
      provincia
    ),
  n = Inf
)


cat("\nSECCIÓN 17.8 COMPLETADA.\n")

