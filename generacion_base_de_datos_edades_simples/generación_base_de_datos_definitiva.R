# ======================================================================
# DEFUNCIONES 2015-2025: FORMATO DE LA BASE DE REFERENCIA
# Columnas: edad | sexo | anio | region | INEC_4 | coverage
#
# Por cada anio, region, INEC_4 y sexo:
#   - edades simples 0, 1, ..., 89
#   - "90 años y más" = suma de todas las edades >= 90
#   - "Ignorada"     = suma de las edades ignoradas
#   - "Total"        = suma de 0:89, 90+ e Ignorada
#
# Los ceros se completan, y Hombre/Mujer se alternan por edad.
# INEC_4 se toma directamente del codigo REDATAM, sin diccionario.
# ======================================================================

library(readODS)
library(data.table)
library(stringr)

# 1. CONFIGURACION ------------------------------------------------------
carpeta <- "C:/Users/User/Desktop/DATOS X"
anios <- 2015:2025
provincias <- c("San José", "Alajuela", "Cartago", "Heredia",
                "Guanacaste", "Puntarenas", "Limón")
sexos <- c("Hombre", "Mujer")
edades <- c(as.character(0:89), "90 años y más", "Ignorada", "Total")

archivo_csv <- file.path(carpeta, "defunciones_edades_simples_2015_2025.csv")
archivo_rds <- file.path(carpeta, "defunciones_edades_simples_2015_2025.rds")
cargar_en_R <- TRUE  # Cambia a FALSE si no tienes memoria suficiente.

# 2. LOCALIZAR LOS 22 ARCHIVOS ODS --------------------------------------
archivos <- list.files(carpeta, pattern = "\\.ods$", full.names = TRUE,
                       ignore.case = TRUE)
patron <- regex("^DEFUN_CM_(201[5-9]|202[0-5])_([HM])(\\([0-9]+\\))?\\.ods$",
                ignore_case = TRUE)
meta <- str_match(basename(archivos), patron)
archivos <- archivos[!is.na(meta[, 1])]
meta <- meta[!is.na(meta[, 1]), , drop = FALSE]

id <- paste(meta[, 2], toupper(meta[, 3]), sep = "_")
if (anyDuplicated(id)) {
  stop("Hay más de un archivo para el mismo año y sexo: ",
       paste(unique(id[duplicated(id)]), collapse = ", "))
}
esperados <- as.vector(outer(anios, c("H", "M"), paste, sep = "_"))
faltantes <- setdiff(esperados, id)
if (length(faltantes)) {
  stop("Faltan archivos: ", paste(faltantes, collapse = ", "))
}

# 3. IMPORTAR CADA ODS SIN PERDER EDADES IGNORADAS -----------------------
leer_archivo <- function(ruta, anio, sexo) {
  x <- read_ods(ruta, sheet = 1, col_names = TRUE, col_types = NA)
  if (ncol(x) < 4L) stop("El ODS debe tener al menos 4 columnas: ", ruta)
  
  # Estructura del ODS adjunto:
  # [1] codigo REDATAM, [2] edad, [3] provincia, [4] Valor
  codigo_txt <- trimws(as.character(x[[1]]))
  edad_txt <- trimws(as.character(x[[2]]))
  region <- trimws(as.character(x[[3]]))
  valor_txt <- trimws(as.character(x[[4]]))
  
  if (anyNA(codigo_txt) || any(!grepl("^[0-9]+$", codigo_txt))) {
    stop("Codigos REDATAM no numericos en ", basename(ruta))
  }
  INEC_4 <- as.integer(codigo_txt)
  if (anyNA(INEC_4)) stop("Codigo REDATAM fuera del rango entero: ", ruta)
  
  if (anyNA(region) || any(!region %in% provincias)) {
    stop("Provincia no reconocida en ", basename(ruta), ": ",
         paste(head(unique(region[is.na(region) | !region %in% provincias]), 5),
               collapse = ", "))
  }
  
  # En la base original '-' significa cero defunciones.
  valor_txt[valor_txt %in% c("-", "–", "—")] <- "0"
  if (anyNA(valor_txt) || any(!grepl("^[0-9]+$", valor_txt))) {
    stop("Valores de defunciones no validos en ", basename(ruta))
  }
  coverage <- as.integer(valor_txt)
  if (anyNA(coverage)) stop("Conteos fuera del rango entero: ", ruta)
  
  # Al importar TODAS las columnas como texto, 'Ignorada' no se hace NA.
  if (anyNA(edad_txt)) stop("Hay edades vacias en ", basename(ruta))
  es_ignorada <- grepl("ignorad", tolower(edad_txt), fixed = TRUE)
  es_total <- tolower(edad_txt) %in% c("total", "totales")
  es_numerica <- grepl("^[0-9]+$", edad_txt)
  
  if (any(!(es_ignorada | es_total | es_numerica))) {
    stop("Edad no reconocida en ", basename(ruta), ": ",
         paste(head(unique(edad_txt[!(es_ignorada | es_total | es_numerica)]), 5),
               collapse = ", "))
  }
  
  edad <- rep(NA_character_, length(edad_txt))
  n <- as.integer(edad_txt[es_numerica])
  if (anyNA(n)) stop("Una edad numerica es demasiado grande en: ", ruta)
  edad[es_numerica] <- ifelse(n >= 90L, "90 años y más", as.character(n))
  edad[es_ignorada] <- "Ignorada"
  
  # Si algun ODS incluye totales originales, no se suman dos veces.
  conservar <- !es_total
  data.table(edad = edad[conservar], sexo = sexo, anio = anio,
             region = region[conservar], INEC_4 = INEC_4[conservar],
             coverage = coverage[conservar])
}

# 4. PROCESAR CADA AÑO POR SEPARADO (MENOR USO DE MEMORIA) --------------
if (file.exists(archivo_csv)) file.remove(archivo_csv)

for (j in seq_along(anios)) {
  y <- anios[j]
  cat("Procesando", y, "...\n")
  
  ruta_h <- archivos[match(paste0(y, "_H"), id)]
  ruta_m <- archivos[match(paste0(y, "_M"), id)]
  
  origen <- rbindlist(list(
    leer_archivo(ruta_h, y, "Hombre"),
    leer_archivo(ruta_m, y, "Mujer")
  ), use.names = TRUE)
  
  # Agregacion efectiva: varios registros >=90 pasan a una sola fila.
  # Las edades ignoradas tambien se suman por causa/region/sexo.
  conteos <- origen[, .(coverage = sum(coverage)),
                    by = .(region, INEC_4, sexo, edad)]
  
  # Total de defunciones para CADA causa, sexo y provincia del año.
  totales <- conteos[, .(coverage = sum(coverage)),
                     by = .(region, INEC_4, sexo)]
  totales[, edad := "Total"]
  conteos <- rbindlist(list(conteos, totales), use.names = TRUE)
  
  # Base completa: todas las causas presentes en el año, todas las
  # provincias, ambos sexos y todas las 93 categorias de edad.
  causas <- sort(unique(origen$INEC_4))
  datos_anio <- CJ(region = provincias, INEC_4 = causas,
                   edad = edades, sexo = sexos, sorted = FALSE)
  datos_anio[, coverage := 0L]
  
  # Sobrescribir solamente las combinaciones con valores observados.
  datos_anio[conteos,
             on = .(region, INEC_4, edad, sexo),
             coverage := i.coverage]
  datos_anio[, anio := as.integer(y)]
  
  # Orden del ejemplo: año, provincia, INEC_4, edad y sexo alternado.
  datos_anio[, orden_region := match(region, provincias)]
  datos_anio[, orden_edad := match(edad, edades)]
  datos_anio[, orden_sexo := match(sexo, sexos)]
  setorder(datos_anio, orden_region, INEC_4, orden_edad, orden_sexo)
  datos_anio[, c("orden_region", "orden_edad", "orden_sexo") := NULL]
  setcolorder(datos_anio,
              c("edad", "sexo", "anio", "region", "INEC_4", "coverage"))
  
  # 5. VALIDACIONES -----------------------------------------------------
  total_fuente <- sum(origen$coverage)
  total_tabla <- sum(datos_anio[edad == "Total", coverage])
  if (total_fuente != total_tabla) {
    stop("Descuadre en ", y, ": original=", total_fuente,
         ", calculado=", total_tabla)
  }
  
  comprobacion <- datos_anio[, .(
    total = coverage[edad == "Total"],
    suma_edades = sum(coverage[edad != "Total"])
  ), by = .(region, INEC_4, sexo)]
  if (any(comprobacion$total != comprobacion$suma_edades)) {
    stop("Hay un total por causa/sexo/provincia incorrecto en ", y)
  }
  
  # CSV unificado: se agrega un año a la vez para ahorrar RAM.
  fwrite(datos_anio, archivo_csv, append = (j > 1L),
         col.names = (j == 1L), bom = (j == 1L))
  cat("  Filas:", format(nrow(datos_anio), big.mark = "."),
      "| Defunciones:", format(total_tabla, big.mark = "."), "\n")
  
  rm(origen, conteos, totales, datos_anio, comprobacion)
  invisible(gc(verbose = FALSE))
}

# 6. CARGAR LA BASE UNIFICADA EN R Y GUARDAR RDS ------------------------
if (cargar_en_R) {
  datos <- fread(archivo_csv, encoding = "UTF-8")
  saveRDS(datos, archivo_rds)
  cat("\nObjeto 'datos' cargado en R.\n")
  print(head(datos, 10))
}
cat("\nArchivo CSV guardado en:\n", archivo_csv, "\n")
if (cargar_en_R) cat("Archivo RDS guardado en:\n", archivo_rds, "\n")

datos <- datos %>%
  mutate(
    INEC_4 = str_sub(as.character(INEC_4), 1, -2)
  )

# ============================================================
# 7. IMPORTAR EL DICCIONARIO CORREGIDO
# ============================================================

diccionario <- read.csv(
  "C:/Users/User/Desktop/DATOS X/diccionario inec 4 cie 10 por año.csv",
  colClasses = "character",
  check.names = FALSE,
  fileEncoding = "UTF-8"
)

# ============================================================
# 8. PREPARAR LAS EQUIVALENCIAS
# ============================================================

diccionario_limpio <- diccionario %>%
  transmute(
    clave_anio = trimws(anio),
    clave_inec = trimws(INEC_4),
    cie10_4 = trimws(CIE10_4)
  ) %>%
  distinct()

# ============================================================
# 9. COMPROBAR QUE NO HAYA EQUIVALENCIAS AMBIGUAS
# ============================================================

conflictos <- diccionario_limpio %>%
  count(clave_anio, clave_inec) %>%
  filter(n > 1)

if (nrow(conflictos) > 0) {
  stop("Existen códigos INEC_4 con varias equivalencias CIE10_4")
}

# ============================================================
# 10. AGREGAR cie10_4 A LA BASE datos
# ============================================================

filas_originales <- nrow(datos)

datos <- datos %>%
  select(-any_of("cie10_4")) %>%
  mutate(
    clave_anio = trimws(as.character(año)),
    clave_inec = trimws(as.character(INEC_4))
  ) %>%
  left_join(
    diccionario_limpio,
    by = c("clave_anio", "clave_inec"),
    relationship = "many-to-one"
  ) %>%
  select(-clave_anio, -clave_inec)

# Comprobar que no se alteró el número de filas

stopifnot(nrow(datos) == filas_originales)

# ============================================================
# 11. VERIFICAR LAS CORRESPONDENCIAS
# ============================================================

datos %>%
  summarise(
    total = n(),
    correspondencias = sum(!is.na(cie10_4)),
    sin_correspondencia = sum(is.na(cie10_4))
  )

# Mostrar códigos que no encontraron equivalencia

datos %>%
  filter(is.na(cie10_4)) %>%
  distinct(año, INEC_4) %>%
  print(n = 50)


# 12. PREPARAR DICCIONARIO

diccionario_limpio <- diccionario %>%
  transmute(
    INEC_4 = trimws(as.character(INEC_4)),
    anio = trimws(as.character(anio)),
    cie10_4 = trimws(as.character(CIE10_4))
  ) %>%
  distinct()

# 13. VERIFICAR EQUIVALENCIAS DUPLICADAS

conflictos <- diccionario_limpio %>%
  count(INEC_4, anio) %>%
  filter(n > 1)

if (nrow(conflictos) > 0) {
  stop("Existen equivalencias ambiguas en el diccionario")
}

# 14. REALIZAR LEFT JOIN

datos_join <- datos %>%
  select(-any_of("cie10_4")) %>%
  mutate(
    INEC_4 = trimws(as.character(INEC_4)),
    anio = trimws(as.character(anio))
  ) %>%
  left_join(
    diccionario_limpio,
    by = c("INEC_4", "anio"),
    relationship = "many-to-one"
  )

# 15. VERIFICAR RESULTADOS

print(names(datos_join))

print(
  datos_join %>%
    summarise(
      total = n(),
      asignados = sum(!is.na(cie10_4)),
      sin_asignar = sum(is.na(cie10_4))
    )
)

# 16. ABRIR BASE RESULTANTE

View(datos_join)

na_causa=datos_join |> filter(is.na(cie10_4))


Base_def=datos_join |> mutate(cie10_4=
                                case_when(INEC_4=="1009" ~ "A00",
                                          INEC_4=="2155" ~ "V15",
                                          TRUE ~ cie10_4))

# ============================================================
# 17. PREPARAR LAS BASES
# ============================================================

setDT(Base_def)

filas_originales <- nrow(Base_def)

# ============================================================
# 18. CREAR CORRESPONDENCIAS ENTRE EDADES
# ============================================================

# Extraer solamente las edades distintas de Base_def

mapa_edades <- data.table(
  edad = unique(as.character(Base_def$edad))
)

# Adaptar las edades al formato de mortalidad

mapa_edades[, edad_mort := {
  
  x <- trimws(edad)
  x_min <- tolower(x)
  
  numero <- suppressWarnings(as.integer(x))
  
  fifelse(
    x_min %in% c("999", "ignorada", "ignorado",
                 "edad ignorada"),
    "Ignorada",
    
    fifelse(
      (!is.na(numero) & numero >= 90 & numero != 999) |
        x_min %in% c(
          "90+", "90+ años", "90 años y más",
          "90 años y mas", "90 años o más"
        ),
      "90 años y más",
      x
    )
  )
}]

# ============================================================
# 19. PREPARAR LA BASE DE MORTALIDAD
# ============================================================

mortalidad_limpia <- as.data.table(base_mortalidad)[
  , .(
    edad_mort = trimws(as.character(edad)),
    sexo = trimws(as.character(sexo)),
    anio = trimws(as.character(anio)),
    region = trimws(as.character(region)),
    exposicion = as.numeric(as.character(exposicion)),
    mortalidad = as.numeric(as.character(mortalidad)),
    mx = as.numeric(as.character(mx))
  )
]

# ============================================================
# 20. ADAPTAR LAS EDADES DE MORTALIDAD A Base_def
# ============================================================

mortalidad_union <- merge(
  mapa_edades,
  mortalidad_limpia,
  by = "edad_mort",
  allow.cartesian = TRUE
)[
  , .(
    edad, sexo, anio, region,
    exposicion, mortalidad, mx
  )
]

# ============================================================
# 21. VERIFICAR QUE NO HAYA CLAVES DUPLICADAS
# ============================================================

duplicados <- mortalidad_union[
  , .N,
  by = .(edad, sexo, anio, region)
][N > 1]

if (nrow(duplicados) > 0) {
  stop("Existen claves duplicadas en mortalidad_union")
}

# ============================================================
# 22. AGREGAR LAS TRES COLUMNAS DIRECTAMENTE A Base_def
# ============================================================

Base_def[
  mortalidad_union,
  on = .(edad, sexo, anio, region),
  `:=`(
    exposicion = i.exposicion,
    mortalidad = i.mortalidad,
    mx = i.mx
  )
]

# ============================================================
# 23. COMPROBAR QUE NO CAMBIÓ EL NÚMERO DE FILAS
# ============================================================

stopifnot(nrow(Base_def) == filas_originales)

# ============================================================
# 24. VERIFICAR RESULTADOS
# ============================================================

print(names(Base_def))

Base_def[
  , .(
    total_filas = .N,
    mortalidad_asignada = sum(!is.na(mortalidad)),
    mortalidad_sin_asignar = sum(is.na(mortalidad)),
    exposicion_NA = sum(is.na(exposicion)),
    mx_NA = sum(is.na(mx))
  )
] |> print()

# ============================================================
# 25. MOSTRAR LAS PRIMERAS 20 FILAS
# ============================================================

print(
  Base_def[
    1:20,
    .(
      edad, sexo, anio, region,
      INEC_4, coverage, cie10_4,
      exposicion, mortalidad, mx
    )
  ]
)


# ============================================================
# 26. CONFIGURACION Y COMPROBACIONES
# ============================================================

library(data.table)

setDT(Base_def)

filas_iniciales <- nrow(Base_def)

Base_def[, edad := as.character(edad)]
Base_def[, sexo := as.character(sexo)]

# Interruptor del procedimiento
repartir_ignorada <- TRUE

# Exportacion opcional de la base completa a CSV
guardar_csv_base_hmd <- FALSE

edad_conocida <- c(
  as.character(0:89),
  "90 años y más"
)

clave_causa <- c("anio", "region", "sexo", "INEC_4")
clave_general <- c("anio", "region", "sexo")

tol_hmd <- 1e-6

requeridas <- c(
  clave_causa, "edad", "coverage"
)

if (!all(requeridas %in% names(Base_def))) {
  stop(
    "Faltan columnas: ",
    paste(
      setdiff(requeridas, names(Base_def)),
      collapse = ", "
    )
  )
}

if (!is.numeric(Base_def$coverage) ||
    anyNA(Base_def$coverage) ||
    any(!is.finite(Base_def$coverage)) ||
    any(Base_def$coverage < 0)) {
  stop("coverage debe ser numerica, no negativa y sin NA.")
}

if (anyNA(Base_def[, ..requeridas])) {
  stop("Existen NA en columnas necesarias.")
}

if (any(
  !Base_def$edad %chin%
  c(edad_conocida, "Ignorada", "Total")
)) {
  stop("Hay edades no reconocidas. Revisar edad 999.")
}

if (any(
  !Base_def$sexo %chin% c("Hombre", "Mujer")
)) {
  stop("Hay categorias de sexo no reconocidas.")
}

# Preservar los datos observados originales
Base_def[
  , coverage_hmd := as.numeric(coverage)
]

# ============================================================
# 27. CALCULAR DEFUNCIONES POR CAUSA
# ============================================================

resumen_causa <- Base_def[
  , .(
    D_conocidas = sum(
      as.numeric(
        coverage[edad %chin% edad_conocida]
      )
    ),
    D_ign = sum(
      as.numeric(
        coverage[edad == "Ignorada"]
      )
    ),
    D_tot = sum(
      as.numeric(
        coverage[edad == "Total"]
      )
    ),
    n_filas_total = sum(edad == "Total")
  ),
  by = .(anio, region, sexo, INEC_4)
]

# Verificar la consistencia de los totales

if (any(abs(
  resumen_causa$D_tot -
  resumen_causa$D_conocidas -
  resumen_causa$D_ign
) > tol_hmd)) {
  stop("Los totales por causa no coinciden.")
}

if (any(resumen_causa$n_filas_total < 1)) {
  stop("Faltan filas Total en algunos grupos.")
}

# ============================================================
# 28. REDISTRIBUCION PRINCIPAL SEGUN HMD
# ============================================================

if (repartir_ignorada) {
  
  # Conservar la fila Ignorada, pero asignar cero
  Base_def[
    edad == "Ignorada",
    coverage_hmd := 0
  ]
  
  # Factores para grupos con edades conocidas
  factores_causa <- resumen_causa[
    D_conocidas > 0 & D_ign > 0,
    .(
      anio, region, sexo, INEC_4,
      factor_hmd = D_tot / D_conocidas
    )
  ]
  
  # Aplicar:
  # D*(x) = D(x) * D_tot / (D_tot - D_ign)
  
  Base_def[
    factores_causa,
    on = .(anio, region, sexo, INEC_4),
    coverage_hmd := fifelse(
      edad %chin% edad_conocida,
      as.numeric(coverage) * i.factor_hmd,
      coverage_hmd
    )
  ]
  
  # ==========================================================
  # 29. RESPALDO NACIONAL PARA CAUSAS SIN EDADES CONOCIDAS
  # ==========================================================
  
  sin_perfil_local <- resumen_causa[
    D_conocidas == 0 & D_ign > 0,
    .(anio, region, sexo, INEC_4, D_ign)
  ]
  
  cat(
    "Grupos sin edades conocidas localmente:",
    nrow(sin_perfil_local), "\n"
  )
  
  if (nrow(sin_perfil_local) > 0) {
    
    # Identificar causa, sexo y año correspondientes
    claves_nacionales <- unique(
      sin_perfil_local[
        , .(anio, sexo, INEC_4)
      ]
    )
    
    # Perfil nacional de la MISMA causa, sexo y año,
    # utilizando los conteos observados.
    
    perfil_nacional <- Base_def[
      claves_nacionales,
      on = .(anio, sexo, INEC_4),
      nomatch = 0L
    ][
      edad %chin% edad_conocida,
      .(
        D_edad = sum(as.numeric(coverage))
      ),
      by = .(anio, sexo, INEC_4, edad)
    ]
    
    # Defunciones nacionales con edad conocida
    perfil_nacional[
      , D_nacional_conocidas := sum(D_edad),
      by = .(anio, sexo, INEC_4)
    ]
    
    # Proporcion nacional de cada edad
    perfil_nacional[
      D_nacional_conocidas > 0,
      peso_edad := D_edad / D_nacional_conocidas
    ]
    
    # Si no existen edades conocidas en todo el pais,
    # no se puede determinar la distribucion proporcional.
    
    faltan_pesos <- perfil_nacional[
      is.na(peso_edad) | !is.finite(peso_edad)
    ]
    
    if (nrow(faltan_pesos) > 0) {
      
      fwrite(
        unique(
          faltan_pesos[
            , .(anio, sexo, INEC_4)
          ]
        ),
        file.path(
          carpeta,
          "HMD_sin_perfil_nacional.csv"
        )
      )
      
      stop(
        "Existen causas sin edades conocidas ",
        "en todo el pais. Requieren revision manual."
      )
    }
    
    # Asignar distribucion nacional a cada provincia
    # que no tenga edades conocidas.
    
    imputaciones <- merge(
      sin_perfil_local,
      perfil_nacional[
        , .(
          anio, sexo, INEC_4,
          edad, peso_edad
        )
      ],
      by = c("anio", "sexo", "INEC_4"),
      allow.cartesian = TRUE,
      sort = FALSE
    )
    
    # Corregir posibles filas duplicadas por eliminar
    # el ultimo digito del codigo INEC_4.
    
    cantidad_filas <- Base_def[
      sin_perfil_local[
        , .(anio, region, sexo, INEC_4)
      ],
      on = .(anio, region, sexo, INEC_4),
      nomatch = 0L
    ][
      edad %chin% edad_conocida,
      .N,
      by = .(
        anio, region, sexo, INEC_4, edad
      )
    ]
    
    imputaciones <- merge(
      imputaciones,
      cantidad_filas,
      by = c(
        "anio", "region", "sexo", "INEC_4", "edad"
      ),
      all.x = TRUE,
      sort = FALSE
    )
    
    if (anyNA(imputaciones$N) ||
        any(imputaciones$N == 0)) {
      stop("Existen edades sin filas de destino.")
    }
    
    # Cada defuncion ignorada se reparte proporcionalmente.
    # Si existen varias filas para una celda, se divide
    # la imputacion para evitar multiplicar las defunciones.
    
    imputaciones[
      , valor_por_fila := D_ign * peso_edad / N
    ]
    
    Base_def[
      imputaciones,
      on = .(
        anio, region, sexo, INEC_4, edad
      ),
      coverage_hmd := i.valor_por_fila
    ]
    
    fwrite(
      sin_perfil_local,
      file.path(
        carpeta,
        "HMD_grupos_respaldo_nacional.csv"
      )
    )
  }
}

# ============================================================
# 30. VALIDAR LA REDISTRIBUCION POR CAUSA
# ============================================================

control_causa <- Base_def[
  , .(
    total_observado = sum(
      as.numeric(coverage[edad == "Total"])
    ),
    conocido_ajustado = sum(
      coverage_hmd[edad %chin% edad_conocida]
    ),
    ignorado_ajustado = sum(
      coverage_hmd[edad == "Ignorada"]
    ),
    total_fila_ajustada = sum(
      coverage_hmd[edad == "Total"]
    )
  ),
  by = .(anio, region, sexo, INEC_4)
]

control_causa[
  , total_estimado :=
    conocido_ajustado + ignorado_ajustado
]

control_causa[
  , diferencia := total_estimado - total_observado
]

if (any(abs(control_causa$diferencia) > tol_hmd)) {
  
  fwrite(
    control_causa[abs(diferencia) > tol_hmd],
    file.path(carpeta, "HMD_errores_causa.csv")
  )
  
  stop(
    "Algunas causas no conservan sus totales. ",
    "Revisar HMD_errores_causa.csv"
  )
}

if (any(abs(
  control_causa$total_fila_ajustada -
  control_causa$total_observado
) > tol_hmd)) {
  stop("Se modifico una fila Total.")
}

if (repartir_ignorada &&
    any(abs(control_causa$ignorado_ajustado) > tol_hmd)) {
  stop("Quedaron defunciones ignoradas sin repartir.")
}

# ============================================================
# 31. CONSTRUIR MORTALIDAD GENERAL OBSERVADA
# ============================================================

# Se reconstruye sumando las defunciones de todas las
# causas contenidas en coverage.
#
# Es necesario comprobar que esta suma coincida con
# los totales generales originales del INEC.

mortalidad_general_hmd <- Base_def[
  , .(
    mortalidad_observada = sum(
      as.numeric(coverage)
    )
  ),
  by = .(anio, region, sexo, edad)
]

mortalidad_general_hmd[
  , mortalidad_hmd := mortalidad_observada
]

# Calcular componentes de mortalidad general

resumen_general <- mortalidad_general_hmd[
  , .(
    D_conocidas = sum(
      mortalidad_observada[
        edad %chin% edad_conocida
      ]
    ),
    D_ign = sum(
      mortalidad_observada[
        edad == "Ignorada"
      ]
    ),
    D_tot = sum(
      mortalidad_observada[
        edad == "Total"
      ]
    )
  ),
  by = .(anio, region, sexo)
]

if (any(abs(
  resumen_general$D_tot -
  resumen_general$D_conocidas -
  resumen_general$D_ign
) > tol_hmd)) {
  stop("Los totales de mortalidad general no coinciden.")
}

# ============================================================
# 32. REDISTRIBUIR MORTALIDAD GENERAL
# ============================================================

if (repartir_ignorada) {
  
  # El PDF no define un respaldo para grupos generales
  # que carezcan completamente de edades conocidas.
  
  sin_perfil_general <- resumen_general[
    D_conocidas == 0 & D_ign > 0
  ]
  
  if (nrow(sin_perfil_general) > 0) {
    
    fwrite(
      sin_perfil_general,
      file.path(
        carpeta,
        "HMD_general_sin_edades_conocidas.csv"
      )
    )
    
    stop(
      "Existen grupos generales sin edades conocidas. ",
      "Requieren revision manual."
    )
  }
  
  # Ignorada pasa a cero en la distribucion ajustada
  mortalidad_general_hmd[
    edad == "Ignorada",
    mortalidad_hmd := 0
  ]
  
  # Factor proporcional para mortalidad general
  factores_general <- resumen_general[
    D_conocidas > 0 & D_ign > 0,
    .(
      anio, region, sexo,
      factor_hmd = D_tot / D_conocidas
    )
  ]
  
  mortalidad_general_hmd[
    factores_general,
    on = .(anio, region, sexo),
    mortalidad_hmd := fifelse(
      edad %chin% edad_conocida,
      mortalidad_observada * i.factor_hmd,
      mortalidad_hmd
    )
  ]
}

# Comprobar conservacion de totales generales

control_general <- mortalidad_general_hmd[
  , .(
    total_observado = sum(
      mortalidad_observada[edad == "Total"]
    ),
    total_ajustado = sum(
      mortalidad_hmd[edad %chin% edad_conocida]
    ) + sum(
      mortalidad_hmd[edad == "Ignorada"]
    ),
    ignoradas_ajustadas = sum(
      mortalidad_hmd[edad == "Ignorada"]
    ),
    total_fila_ajustada = sum(
      mortalidad_hmd[edad == "Total"]
    )
  ),
  by = .(anio, region, sexo)
]

if (any(abs(
  control_general$total_observado -
  control_general$total_ajustado
) > tol_hmd)) {
  stop("La mortalidad general no conserva los totales.")
}

if (any(abs(
  control_general$total_observado -
  control_general$total_fila_ajustada
) > tol_hmd)) {
  stop("Se altero una fila Total de mortalidad general.")
}

if (repartir_ignorada &&
    any(abs(control_general$ignoradas_ajustadas) > tol_hmd)) {
  stop("Quedaron defunciones ignoradas en mortalidad general.")
}

# ============================================================
# 33. CREAR TOTAL DE AMBOS SEXOS
# ============================================================

# Total = Hombre + Mujer ajustados.
# Se crea por separado, sin agregar filas a Base_def.

general_ambos_sexos <- mortalidad_general_hmd[
  , .(
    mortalidad_observada = sum(
      mortalidad_observada
    ),
    mortalidad_hmd = sum(
      mortalidad_hmd
    )
  ),
  by = .(anio, region, edad)
]

general_ambos_sexos[
  , sexo := "Total"
]

setcolorder(
  general_ambos_sexos,
  c(
    "anio", "region", "sexo", "edad",
    "mortalidad_observada", "mortalidad_hmd"
  )
)

general_todos_sexos <- rbindlist(
  list(
    mortalidad_general_hmd,
    general_ambos_sexos
  ),
  use.names = TRUE
)

# ============================================================
# 34. COMPARACION CAUSAS VS MORTALIDAD GENERAL
# ============================================================

# La suma de causas ajustadas puede diferir de la
# mortalidad general ajustada dentro de cada edad.
# Esta diferencia NO se corrige mediante raking.

suma_causas_hmd <- Base_def[
  edad %chin% edad_conocida,
  .(
    suma_causas_hmd = sum(coverage_hmd)
  ),
  by = .(anio, region, sexo, edad)
]

comparacion_hmd <- merge(
  mortalidad_general_hmd[
    edad %chin% edad_conocida,
    .(
      anio, region, sexo, edad, mortalidad_hmd
    )
  ],
  suma_causas_hmd,
  by = c("anio", "region", "sexo", "edad"),
  all = TRUE,
  sort = FALSE
)

comparacion_hmd[
  , diferencia := suma_causas_hmd - mortalidad_hmd
]

cat(
  "\nMayor diferencia absoluta por edad:",
  max(
    abs(comparacion_hmd$diferencia),
    na.rm = TRUE
  ),
  "\n"
)

# Comparacion opcional con la columna mortalidad
# proveniente de la base externa.

if ("mortalidad" %in% names(Base_def)) {
  
  externa_unica <- unique(
    Base_def[
      edad %chin% edad_conocida &
        !is.na(mortalidad),
      .(anio, region, sexo, edad, mortalidad)
    ]
  )
  
  if (nrow(externa_unica) > 0) {
    
    claves_ext <- c(
      "anio", "region", "sexo", "edad"
    )
    
    ambiguas <- externa_unica[
      , .N,
      by = claves_ext
    ][N > 1]
    
    if (nrow(ambiguas) > 0) {
      
      warning(
        "Existen varios valores de mortalidad externa ",
        "para una misma combinacion de claves."
      )
      
    } else {
      
      control_fuentes <- merge(
        externa_unica,
        mortalidad_general_hmd[
          , c(
            claves_ext,
            "mortalidad_observada"
          ),
          with = FALSE
        ],
        by = claves_ext,
        all.x = TRUE
      )
      
      control_fuentes[
        , diferencia :=
          as.numeric(mortalidad) -
          mortalidad_observada
      ]
      
      n_distintos <- control_fuentes[
        !is.na(diferencia) &
          abs(diferencia) > tol_hmd,
        .N
      ]
      
      cat(
        "Celdas donde la mortalidad externa difiere",
        "de la suma de causas:",
        n_distintos, "\n"
      )
      
      if (n_distintos > 0) {
        
        fwrite(
          control_fuentes[
            !is.na(diferencia) &
              abs(diferencia) > tol_hmd
          ],
          file.path(
            carpeta,
            "HMD_diferencias_fuente_externa.csv"
          )
        )
      }
    }
  }
}

# ============================================================
# 35. RESUMEN Y GUARDADO DEFINITIVO
# ============================================================

resumen_final <- data.table(
  ajuste_activo = repartir_ignorada,
  
  defunciones_totales = Base_def[
    edad == "Total",
    sum(as.numeric(coverage))
  ],
  
  edades_ignoradas_originales = Base_def[
    edad == "Ignorada",
    sum(as.numeric(coverage))
  ],
  
  ignoradas_ajustadas = Base_def[
    edad == "Ignorada",
    sum(coverage_hmd)
  ],
  
  grupos_con_respaldo_nacional =
    if (repartir_ignorada) {
      nrow(
        resumen_causa[
          D_conocidas == 0 & D_ign > 0
        ]
      )
    } else {
      0L
    }
)

cat("\nRESUMEN FINAL DEL AJUSTE HMD\n")
print(resumen_final)

# Confirmar que no se agregaron ni eliminaron filas
stopifnot(
  nrow(Base_def) == filas_iniciales
)

# Organizar las columnas
setcolorder(
  Base_def,
  c(
    "edad", "sexo", "anio", "region",
    "INEC_4", "coverage", "coverage_hmd",
    setdiff(
      names(Base_def),
      c(
        "edad", "sexo", "anio", "region",
        "INEC_4", "coverage", "coverage_hmd"
      )
    )
  )
)

# Exportar resultados generales y diagnosticos
fwrite(
  general_todos_sexos,
  file.path(
    carpeta,
    "MORTALIDAD_GENERAL_HMD.csv"
  ),
  bom = TRUE
)

fwrite(
  comparacion_hmd,
  file.path(
    carpeta,
    "HMD_COMPARACION_CAUSAS_GENERAL.csv"
  ),
  bom = TRUE
)

fwrite(
  resumen_final,
  file.path(
    carpeta,
    "HMD_RESUMEN.csv"
  ),
  bom = TRUE
)

# Opcional: exportar toda la base ajustada
if (guardar_csv_base_hmd) {
  fwrite(
    Base_def,
    file.path(carpeta, "BASE_DEF_HMD.csv"),
    bom = TRUE
  )
}

# Guardar objetos finales de R
save(
  Base_def,
  mortalidad_general_hmd,
  general_todos_sexos,
  resumen_causa,
  control_causa,
  control_general,
  comparacion_hmd,
  file = file.path(
    carpeta,
    "BASE_EDAD_SIMPLE_DEEFINITIVA.RData"
  )
)

cat(
  "\nRedistribucion y validaciones completadas.\n"
)



# ======================================================================
# 36. COMPLETAR TODAS LAS CAUSAS (PARES INEC_4 + cie10_4)
#     EN TODOS LOS ANIOS, PROVINCIAS, SEXOS Y EDADES (2015-2025)
# ======================================================================
# REQUISITO: Base_def generada por las secciones 1-35.
# Si ya corriste una seccion 36 anterior, se excluyen las filas que esta
# identifico como fila_completada_cero = TRUE antes de reconstruir.
# Se conservan las causas como PARES DE CODIGOS, sin NA ni "0" en codigos.
# Solo las defunciones (coverage y coverage_hmd) se ponen a cero en filas nuevas.
# No se inventan codigos faltantes o ambiguos.

library(data.table)
setDT(Base_def)

anios_36 <- 2015:2025

regiones_36 <- c(
  "San José", "Alajuela", "Cartago", "Heredia",
  "Guanacaste", "Puntarenas", "Limón"
)

sexos_36 <- c("Hombre", "Mujer")

edades_36 <- c(
  as.character(0:89),
  "90 años y más",
  "Ignorada",
  "Total"
)

clave_36 <- c(
  "anio", "region", "sexo", "edad",
  "INEC_4", "cie10_4"
)

faltan_36 <- setdiff(
  c(clave_36, "coverage", "coverage_hmd"),
  names(Base_def)
)

if (length(faltan_36)) {
  stop(
    "Faltan columnas: ",
    paste(faltan_36, collapse = ", ")
  )
}

# ============================================================
# 36.1. ELIMINAR FILAS ARTIFICIALES PREVIAS
# ============================================================

# Si se ejecuto una seccion 36 anterior, retirar
# las filas que fueron agregadas artificialmente.

if ("fila_completada_cero" %in% names(Base_def)) {
  
  n_retiradas <- Base_def[
    !is.na(fila_completada_cero) &
      fila_completada_cero == TRUE,
    .N
  ]
  
  Base_def <- copy(
    Base_def[
      is.na(fila_completada_cero) |
        fila_completada_cero == FALSE
    ]
  )
  
  cat(
    "Filas de completaciones previas retiradas:",
    n_retiradas, "\n"
  )
}

# ============================================================
# 36.2. NORMALIZAR VARIABLES
# ============================================================

Base_def[
  , `:=`(
    anio = as.integer(as.character(anio)),
    region = trimws(as.character(region)),
    sexo = trimws(as.character(sexo)),
    edad = trimws(as.character(edad)),
    INEC_4 = trimws(as.character(INEC_4)),
    cie10_4 = trimws(as.character(cie10_4)),
    coverage = as.numeric(as.character(coverage)),
    coverage_hmd = as.numeric(as.character(coverage_hmd))
  )
]

# Identificar codigos invalidos

es_codigo_invalido <- function(x) {
  
  is.na(x) |
    toupper(trimws(x)) %chin%
    c(
      "", "0", "NA", "N/A", "NULL",
      "SIN_EQUIVALENCIA"
    )
}

if (any(es_codigo_invalido(Base_def$INEC_4))) {
  stop(
    "Hay INEC_4 originales vacios, 0 o no validos."
  )
}

if (anyNA(
  Base_def[
    , .(
      anio, region, sexo, edad,
      coverage, coverage_hmd
    )
  ]
)) {
  stop(
    "Hay NA originales en dimensiones o conteos."
  )
}

if (
  any(!Base_def$anio %in% anios_36) ||
  any(!Base_def$region %chin% regiones_36) ||
  any(!Base_def$sexo %chin% sexos_36) ||
  any(!Base_def$edad %chin% edades_36)
) {
  stop(
    "Hay edades, regiones, sexos o anios fuera ",
    "de los valores previstos."
  )
}

# ============================================================
# 36.3. RECUPERAR EQUIVALENCIAS CIE10 FALTANTES
# ============================================================

# Primero buscar equivalencias validas dentro
# del mismo anio.

refs_36 <- unique(
  Base_def[
    !es_codigo_invalido(cie10_4),
    .(anio, INEC_4, cie10_4)
  ]
)

mapa_anio_36 <- refs_36[
  , .(
    n_cie = uniqueN(cie10_4),
    cie_ref = cie10_4[1L]
  ),
  by = .(anio, INEC_4)
][
  n_cie == 1L,
  .(anio, INEC_4, cie_ref)
]

Base_def[
  mapa_anio_36,
  on = .(anio, INEC_4),
  cie10_4 := fifelse(
    es_codigo_invalido(cie10_4),
    i.cie_ref,
    cie10_4
  )
]

# Si no hay equivalencia dentro del mismo anio,
# buscar en el conjunto completo 2015-2025.

mapa_global_36 <- refs_36[
  , .(
    n_cie = uniqueN(cie10_4),
    cie_ref = cie10_4[1L]
  ),
  by = INEC_4
][
  n_cie == 1L,
  .(INEC_4, cie_ref)
]

Base_def[
  mapa_global_36,
  on = "INEC_4",
  cie10_4 := fifelse(
    es_codigo_invalido(cie10_4),
    i.cie_ref,
    cie10_4
  )
]

# Si no se puede recuperar un CIE10 sin ambiguedad,
# detener el procedimiento.

sin_cie_36 <- unique(
  Base_def[
    es_codigo_invalido(cie10_4),
    .(anio, INEC_4)
  ]
)

if (nrow(sin_cie_36)) {
  
  archivo_revision_36 <- file.path(
    carpeta,
    "HMD_codigos_CIE10_por_revisar.csv"
  )
  
  fwrite(
    sin_cie_36,
    archivo_revision_36
  )
  
  stop(
    "Hay ", nrow(sin_cie_36),
    " pares anio-INEC_4 sin CIE-10 ",
    "recuperable de forma unica. Revisa: ",
    archivo_revision_36
  )
}

# ============================================================
# 36.4. OBTENER TODAS LAS PAREJAS DE CAUSAS
# ============================================================

# Cada pareja INEC_4 + cie10_4 se considera
# una causa que debe aparecer en todos los anios.

# No se cruzan arbitrariamente los codigos INEC
# con todos los CIE10.

pares_36 <- unique(
  Base_def[
    , .(INEC_4, cie10_4)
  ]
)

setorder(
  pares_36,
  INEC_4,
  cie10_4
)

n_original_36 <- nrow(Base_def)

muertes_originales_36 <- Base_def[
  edad == "Total",
  sum(coverage)
]

muertes_hmd_originales_36 <- Base_def[
  edad == "Total",
  sum(coverage_hmd)
]

Base_def[
  , fila_completada_cero := FALSE
]

# ============================================================
# 36.5. PREPARAR VARIABLES GENERALES
# ============================================================

# Estas variables corresponden al anio, region,
# sexo y edad; no dependen de la causa especifica.

clave_general_36 <- c(
  "anio", "region", "sexo", "edad"
)

variables_generales_36 <- intersect(
  c(
    "exposicion",
    "mortalidad",
    "mx",
    "mortalidad_general_hmd"
  ),
  names(Base_def)
)

referencias_generales_36 <- list()

for (v in variables_generales_36) {
  
  t <- unique(
    Base_def[
      !is.na(get(v)),
      c(clave_general_36, v),
      with = FALSE
    ]
  )
  
  if (anyDuplicated(
    t,
    by = clave_general_36
  )) {
    stop(
      "La variable general '", v,
      "' tiene valores contradictorios ",
      "para la misma combinacion ",
      "anio-region-sexo-edad."
    )
  }
  
  referencias_generales_36[[v]] <- t
}

# ============================================================
# 36.6. COMPLETAR TODAS LAS COMBINACIONES
# ============================================================

# Se generan todas las combinaciones de:
# anio x region x sexo x edad x pareja de causa.

# Se trabaja por anio y bloques de 50 parejas
# para reducir el uso de memoria.

partes_36 <- list()
n_partes_36 <- 0L

for (y in anios_36) {
  
  existentes_anio_36 <- unique(
    Base_def[
      anio == y,
      .(
        region, sexo, edad,
        INEC_4, cie10_4
      )
    ]
  )
  
  for (
    inicio in seq.int(
      1L,
      nrow(pares_36),
      by = 50L
    )
  ) {
    
    ids <- inicio:min(
      inicio + 49L,
      nrow(pares_36)
    )
    
    grilla <- CJ(
      region = regiones_36,
      sexo = sexos_36,
      edad = edades_36,
      id_par = ids,
      sorted = FALSE
    )
    
    grilla[
      , `:=`(
        INEC_4 = pares_36$INEC_4[id_par],
        cie10_4 = pares_36$cie10_4[id_par]
      )
    ]
    
    grilla[
      , id_par := NULL
    ]
    
    # Equivalente a complete() con
    # nesting(INEC_4, cie10_4).
    
    # Conservar solamente las combinaciones
    # que NO existen en Base_def.
    
    nuevas <- grilla[
      !existentes_anio_36,
      on = .(
        region, sexo, edad,
        INEC_4, cie10_4
      )
    ]
    
    if (nrow(nuevas)) {
      
      nuevas[
        , `:=`(
          anio = as.integer(y),
          coverage = 0,
          coverage_hmd = 0,
          fila_completada_cero = TRUE
        )
      ]
      
      # Recuperar exposicion, mortalidad y mx
      # cuando exista una referencia valida.
      
      for (v in variables_generales_36) {
        
        ref <- referencias_generales_36[[v]]
        
        if (nrow(ref)) {
          
          nuevas[
            ref,
            on = .(
              anio, region, sexo, edad
            ),
            (v) := get(paste0("i.", v))
          ]
        }
      }
      
      # Banderas auxiliares antiguas.
      
      for (
        v in intersect(
          c(
            "cie10_inferido_de_otro_anio",
            "cie10_pendiente_revision"
          ),
          names(Base_def)
        )
      ) {
        
        nuevas[
          , (v) := FALSE
        ]
      }
      
      n_partes_36 <- n_partes_36 + 1L
      
      partes_36[[n_partes_36]] <- nuevas
    }
  }
  
  cat(
    "Anio completado:", y, "\n"
  )
}

# ============================================================
# 36.7. UNIR LAS FILAS NUEVAS CON Base_def
# ============================================================

n_nuevas_36 <- sum(
  vapply(
    partes_36,
    nrow,
    integer(1)
  )
)

if (n_nuevas_36 > 0L) {
  
  nuevas_todas_36 <- rbindlist(
    partes_36,
    use.names = TRUE,
    fill = TRUE
  )
  
  Base_def <- rbindlist(
    list(
      Base_def,
      nuevas_todas_36
    ),
    use.names = TRUE,
    fill = TRUE
  )
  
  rm(nuevas_todas_36)
}

rm(partes_36)

# ============================================================
# 36.8. VALIDACIONES FINALES
# ============================================================

# Calcular numero esperado de combinaciones.

filas_por_par_36 <- length(anios_36) *
  length(regiones_36) *
  length(sexos_36) *
  length(edades_36)

numero_combinaciones_36 <- uniqueN(
  Base_def,
  by = clave_36
)

numero_esperado_36 <- nrow(pares_36) *
  filas_por_par_36

if (
  numero_combinaciones_36 != numero_esperado_36
) {
  stop(
    "No se completaron todas las ",
    "combinaciones esperadas."
  )
}

# No permitir codigos vacios o invalidos.

if (
  any(es_codigo_invalido(Base_def$INEC_4)) ||
  any(es_codigo_invalido(Base_def$cie10_4))
) {
  stop(
    "Hay un codigo de causa NA, vacio o 0: ",
    "resultado invalido."
  )
}

# No permitir NA en los conteos.

if (anyNA(
  Base_def[
    , .(coverage, coverage_hmd)
  ]
)) {
  stop(
    "Hay NA en los conteos de defunciones."
  )
}

# Comprobar que no cambiaron las defunciones.

if (
  !isTRUE(
    all.equal(
      muertes_originales_36,
      Base_def[
        edad == "Total",
        sum(coverage)
      ],
      tolerance = 1e-8
    )
  ) ||
  !isTRUE(
    all.equal(
      muertes_hmd_originales_36,
      Base_def[
        edad == "Total",
        sum(coverage_hmd)
      ],
      tolerance = 1e-8
    )
  )
) {
  stop(
    "Se alteraron las defunciones totales ",
    "al completar."
  )
}

# ============================================================
# 36.9. ORDENAR LA BASE DEFINITIVA
# ============================================================

Base_def[
  , orden_region_36 := match(
    region,
    regiones_36
  )
]

Base_def[
  , orden_edad_36 := match(
    edad,
    edades_36
  )
]

Base_def[
  , orden_sexo_36 := match(
    sexo,
    sexos_36
  )
]

setorder(
  Base_def,
  anio,
  orden_region_36,
  INEC_4,
  cie10_4,
  orden_edad_36,
  orden_sexo_36
)

Base_def[
  , c(
    "orden_region_36",
    "orden_edad_36",
    "orden_sexo_36"
  ) := NULL
]

setcolorder(
  Base_def,
  c(
    "edad",
    "sexo",
    "anio",
    "region",
    "INEC_4",
    "cie10_4",
    setdiff(
      names(Base_def),
      c(
        "edad", "sexo", "anio",
        "region", "INEC_4", "cie10_4"
      )
    )
  )
)

# ============================================================
# 36.10. MOSTRAR RESUMEN Y GUARDAR
# ============================================================

cat(
  "\nPares INEC_4 + cie10_4:",
  nrow(pares_36), "\n"
)

cat(
  "Filas originales:",
  n_original_36, "\n"
)

cat(
  "Filas nuevas con cero defunciones:",
  n_nuevas_36, "\n"
)

cat(
  "Filas definitivas:",
  nrow(Base_def), "\n"
)

# Verificar causas presentes por anio.

print(
  Base_def[
    , .(
      pares_causa = uniqueN(.SD),
      filas = .N,
      filas_agregadas = sum(
        fila_completada_cero
      )
    ),
    by = anio,
    .SDcols = c("INEC_4", "cie10_4")
  ][order(anio)]
)

# Guardar sin sobrescribir el archivo anterior.

archivo_final_36 <- file.path(
  carpeta,
  "BASE_DEF_HMD_CAUSAS_COMPLETAS_2015_2025.RData"
)

save(
  Base_def,
  file = archivo_final_36
)

cat(
  "Base guardada en:",
  archivo_final_36, "\n"
)



fwrite(
  Base_def,
  "C:/Users/User/Desktop/DATOS X/BASE_DEF_HMD_CAUSAS_COMPLETAS_2015_2025.csv",
  bom = TRUE
)
