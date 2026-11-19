# =============================================================================
# FUNCIONES | DEFUNCIONES POR CAUSA Y EDAD SIMPLE | COSTA RICA 2015-2025
# =============================================================================
# Funciones que usa script_completo_corregido_2015_2025.R. Este archivo solo
# define funciones: no lee datos ni escribe archivos al cargarlo.
#
# Organización:
#   A. Configuración y validación de entradas
#   B. Utilidades pequeñas (relleno de etiquetas, códigos, conteos, decimales)
#   C. Lectura de exposición, diccionario y matrices REDATAM anuales
#   D. Base por causa y edad simple (secciones 1-5 del script original)
#   E. Reparto de la edad ignorada (sección 6 del script original)
#
# Ninguna función usa '<<-': cada una recibe lo que necesita como argumento y
# devuelve sus resultados. Los archivos se escriben solo en las funciones
# cuyo nombre empieza con 'escribir_' o que lo dicen en su documentación.
# =============================================================================


# -----------------------------------------------------------------------------
# A. CONFIGURACIÓN Y VALIDACIÓN DE ENTRADAS
# -----------------------------------------------------------------------------

#' Verifica que estén instalados los paquetes requeridos y los carga.
cargar_paquetes <- function(paquetes = c('data.table', 'readxl')) {
  faltan <- paquetes[!vapply(paquetes, requireNamespace, logical(1),
                             quietly = TRUE)]
  if (length(faltan)) {
    stop('Faltan paquetes de R: ', paste(faltan, collapse = ', '),
         '\nInstálelos una vez con: install.packages(c(',
         paste0("'", faltan, "'", collapse = ', '), '))', call. = FALSE)
  }
  suppressPackageStartupMessages(
    for (p in paquetes) library(p, character.only = TRUE)
  )
  invisible(TRUE)
}

#' Devuelve el valor de una opción, en este orden de prioridad:
#'   1. argumento de línea de comandos '--nombre=valor'
#'   2. variable de entorno 'variable_entorno'
#'   3. 'por_defecto'
#' Ejemplo: Rscript script.R --entrada=/datos/redatam --salida=/datos/salida
opcion <- function(nombre, variable_entorno, por_defecto = NULL,
                   args = commandArgs(trailingOnly = TRUE)) {
  prefijo <- paste0('--', nombre, '=')
  en_args <- args[startsWith(args, prefijo)]
  if (length(en_args)) return(substring(en_args[length(en_args)],
                                        nchar(prefijo) + 1L))
  en_entorno <- Sys.getenv(variable_entorno, unset = '')
  if (nzchar(en_entorno)) return(en_entorno)
  por_defecto
}

#' Completa y valida la configuración del usuario.
#'
#' @param config lista con carpeta_entrada, carpeta_salida (NULL = subcarpeta
#'   'salida_edad_simple' dentro de la entrada), archivo_referencia, anios,
#'   completar_ceros, guardar_version_compacta y repartir_ignoradas.
#' @return la misma lista con las constantes del análisis y todas las rutas de
#'   entrada y salida. No crea carpetas ni lee datos.
preparar_configuracion <- function(config) {
  requeridos <- c('carpeta_entrada', 'anios', 'completar_ceros',
                  'guardar_version_compacta', 'repartir_ignoradas')
  falta <- setdiff(requeridos, names(config))
  if (length(falta))
    stop('Configuración incompleta, falta: ', paste(falta, collapse = ', '),
         call. = FALSE)

  for (bandera in c('completar_ceros', 'guardar_version_compacta',
                    'repartir_ignoradas')) {
    if (!isTRUE(config[[bandera]]) && !isFALSE(config[[bandera]]))
      stop('La opción ', bandera, ' debe ser TRUE o FALSE.', call. = FALSE)
  }
  if (!is.numeric(config$anios) || !length(config$anios) ||
      anyNA(config$anios) || any(config$anios != round(config$anios)))
    stop('anios debe ser un vector de años enteros, p. ej. 2015:2025.',
         call. = FALSE)
  config$anios <- as.integer(config$anios)

  # El reparto (sección 6) necesita todas las filas cero por causa.
  if (config$repartir_ignoradas && !config$completar_ceros)
    stop('El reparto de la edad ignorada requiere completar_ceros = TRUE.',
         call. = FALSE)

  carpeta <- config$carpeta_entrada
  if (!dir.exists(carpeta))
    stop('No existe la carpeta de entrada: ',
         normalizePath(carpeta, mustWork = FALSE),
         '\nIndíquela con --entrada=RUTA o con la variable de entorno ',
         'CR_MORT_EDAD_SIMPLE_ENTRADA.', call. = FALSE)

  if (is.null(config$carpeta_salida))
    config$carpeta_salida <- file.path(carpeta, 'salida_edad_simple')
  salida <- config$carpeta_salida

  # Constantes del análisis (idénticas al script original).
  config$provincias <- c('San José', 'Alajuela', 'Cartago', 'Heredia',
                         'Guanacaste', 'Puntarenas', 'Limón')
  config$sexos <- c('Hombre', 'Mujer')
  config$edades <- c(as.character(0:89), '90 años y más', 'Ignorada', 'Total')
  config$edades_conocidas <- c(as.character(0:89), '90 años y más')
  config$llaves <- c('anio', 'region', 'INEC_4', 'edad', 'sexo')

  # Rutas de entrada.
  config$entrada <- list(
    defunciones = setNames(file.path(carpeta, paste0('def_', config$anios,
                                                     '.xlsx')),
                           config$anios),
    exposicion  = file.path(carpeta, 'mortalidad.csv'),
    diccionario = file.path(carpeta, 'diccionario_inec_cie10_por_anio.csv'),
    referencia  = resolver_referencia(config$archivo_referencia, carpeta)
  )

  # Rutas de salida.
  config$salida <- list(
    consolidada  = file.path(salida,
      'mortalidad_inec_cie10_mx_edad_simple_2015_2025.csv'),
    compacta     = file.path(salida,
      'mortalidad_inec_cie10_mx_edad_simple_2015_2025_solo_positivas.csv'),
    anual        = function(anio) file.path(salida,
      paste0('mortalidad_inec_cie10_mx_edad_simple_', anio, '.csv')),
    control_totales   = file.path(salida,
      'control_totales_edad_simple_2015_2025.csv'),
    control_csv       = file.path(salida,
      'control_diferencias_con_mortalidad_2.csv'),
    excepciones       = file.path(salida,
      'excepciones_diccionario_edad_simple.csv'),
    top10             = file.path(salida,
      'top10_causas_edad_simple_2015_2025.csv'),
    top10_sexo        = file.path(salida,
      'top10_causas_sexo_edad_simple_2015_2025.csv'),
    repartida         = file.path(salida,
      'mortalidad_inec_cie10_mx_edad_simple_2015_2025_REPARTIDA.csv'),
    control_fuentes   = file.path(salida,
      'control_diferencias_observadas_y_repartidas.csv'),
    sin_repartir      = file.path(salida,
      'control_edades_ignoradas_sin_distribuir.csv')
  )
  config
}

#' Ruta del archivo de referencia para el reparto de la edad ignorada.
#' Si no se indica, busca el nombre limpio y, si no existe, el nombre con que
#' se descargó originalmente ('...(in) (1).csv').
resolver_referencia <- function(archivo, carpeta) {
  if (!is.null(archivo) && nzchar(archivo)) {
    # Un nombre sin carpeta se busca dentro de la carpeta de entrada.
    if (dirname(archivo) == '.') archivo <- file.path(carpeta, archivo)
    return(archivo)
  }
  candidatos <- file.path(carpeta, c(
    'mortalidad_mx_1000_2015_2025_edad_repartida.csv',
    'mortalidad_mx_1000_2015_2025_edad_repartida(in) (1).csv'
  ))
  existe <- file.exists(candidatos)
  if (any(existe)) candidatos[which(existe)[1]] else candidatos[1]
}

#' Detiene la corrida antes de empezar si falta algún archivo de entrada.
verificar_entradas <- function(cfg) {
  necesarios <- c(cfg$entrada$defunciones, cfg$entrada$exposicion,
                  cfg$entrada$diccionario)
  if (cfg$repartir_ignoradas) necesarios <- c(necesarios,
                                              cfg$entrada$referencia)
  faltantes <- necesarios[!file.exists(necesarios)]
  if (length(faltantes))
    stop('No se encuentran estos archivos:\n',
         paste(faltantes, collapse = '\n'), call. = FALSE)
  invisible(TRUE)
}

#' Crea la carpeta de salida y comprueba que se pueda escribir en ella.
preparar_carpeta_salida <- function(carpeta) {
  dir.create(carpeta, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(carpeta) || file.access(carpeta, 2L) != 0L)
    stop('No se puede escribir en la carpeta de salida: ', carpeta,
         call. = FALSE)
  invisible(carpeta)
}

#' Borra un archivo si existe (las salidas acumulativas se escriben con append).
borrar_si_existe <- function(archivo) {
  if (file.exists(archivo)) file.remove(archivo)
  invisible(archivo)
}

#' Ejecuta 'expr' y, si falla, antepone el contexto al mensaje de error.
con_contexto <- function(contexto, expr) {
  tryCatch(expr, error = function(e)
    stop(contexto, ': ', conditionMessage(e), call. = FALSE))
}

paso <- function(...) message('\n== ', ..., ' ==')

#' Número con punto de miles para los mensajes (1.234.567).
miles <- function(x) format(x, big.mark = '.', decimal.mark = ',')


# -----------------------------------------------------------------------------
# B. UTILIDADES PEQUEÑAS
# -----------------------------------------------------------------------------

#' Rellena hacia abajo una columna de etiquetas (sexo o edad del Excel
#' REDATAM), sin alterar las columnas de causa.
arrastrar <- function(x) {
  x <- trimws(as.character(x))
  x[!is.na(x) & !nzchar(x)] <- NA_character_
  i <- seq_along(x)
  ultimo <- cummax(ifelse(!is.na(x), i, 0L))
  y <- rep(NA_character_, length(x))
  ok <- ultimo > 0L
  y[ok] <- x[ultimo[ok]]
  y
}

#' INEC_4 -> CIE-10 de 3 caracteres: 921 -> I21, 1044 -> J44, 2155 -> U55.
inec_a_cie10 <- function(inec4) {
  n <- as.integer(inec4)
  letra <- n %/% 100L
  if (anyNA(n) || any(!(letra %in% 1:26)))
    stop('INEC_4 fuera de rango: revise los códigos.')
  paste0(LETTERS[letra], sprintf('%02d', n %% 100L))
}

#' Número en texto con 10 decimales fijos (sin notación científica).
fijo <- function(x) {
  res <- rep(NA_character_, length(x))
  sel <- !is.na(x) & is.finite(x)
  res[sel] <- sprintf('%.10f', x[sel])
  res
}

#' Conteo del Excel a entero: '-', '' y NA valen 0; cualquier otro texto no
#' numérico detiene la corrida.
valor_num <- function(x, etiqueta) {
  x <- trimws(as.character(x))
  x[is.na(x) | x %in% c('-', '')] <- '0'
  malo <- !grepl('^[0-9]+$', x)
  if (any(malo))
    stop('Conteo no numérico en ', etiqueta, ': ',
         paste(unique(x[malo]), collapse = ', '))
  as.integer(x)
}

#' rbindlist que devuelve una tabla vacía con columnas conocidas si no hay
#' filas (para que los controles se escriban siempre con encabezado).
unir_o_vacia <- function(lista, vacia) {
  res <- rbindlist(lista, fill = TRUE)
  if (!ncol(res)) vacia else res
}


# -----------------------------------------------------------------------------
# C. LECTURA DE INSUMOS
# -----------------------------------------------------------------------------

#' Lee la exposición por edad, sexo, año y provincia (mortalidad.csv).
#'
#' @return lista con:
#'   exposicion: tabla edad/sexo/anio/region/exposicion, con la edad 'Total'
#'     reconstruida como 0..89 + 90+ (sin Ignorada).
#'   control_csv: muertes del CSV, solo para compararlas con el Excel.
leer_exposicion <- function(archivo, cfg) {
  sexos <- cfg$sexos
  exp_raw <- fread(archivo, encoding = 'UTF-8', na.strings = c('NA', ''),
                   colClasses = list(character = c('edad', 'sexo', 'region')))
  columnas_exp <- c('edad', 'sexo', 'anio', 'region', 'exposicion',
                    'mortalidad')
  if (!all(columnas_exp %in% names(exp_raw)))
    stop(basename(archivo), ' no contiene las columnas esperadas: ',
         paste(setdiff(columnas_exp, names(exp_raw)), collapse = ', '))

  # Los totales de muertes y mx del CSV NO sustituyen los del Excel original.
  control_csv <- exp_raw[sexo %in% sexos,
    .(edad, sexo, anio = as.integer(anio), region,
      mortalidad_csv = as.numeric(mortalidad))]
  exp <- exp_raw[sexo %in% sexos,
    .(edad, sexo, anio = as.integer(anio), region,
      exposicion = as.numeric(exposicion))]
  exp <- exp[anio %in% cfg$anios]

  if (anyDuplicated(exp, by = c('edad', 'sexo', 'anio', 'region')))
    stop('Hay duplicados en las llaves edad/sexo/año/provincia de exposición.')
  if (!setequal(unique(exp$edad), cfg$edades[cfg$edades != 'Total']))
    stop('Las edades de ', basename(archivo),
         ' no corresponden a 0..89, 90+, Ignorada.')
  if (!setequal(unique(exp$region), cfg$provincias))
    stop('Las provincias de exposición no son las siete esperadas.')
  if (exp[edad != 'Ignorada', anyNA(exposicion)])
    stop('Exposiciones NA en edades conocidas. Revisar ', basename(archivo), '.')

  # La población total por sexo/provincia/año suma 0..89 más 90+; sin Ignorada.
  exp_total <- exp[edad != 'Ignorada',
    .(exposicion = sum(exposicion)), by = .(anio, region, sexo)]
  exp_total[, edad := 'Total']
  exp <- rbind(exp, exp_total[, names(exp), with = FALSE])
  setkeyv(exp, c('edad', 'sexo', 'anio', 'region'))

  list(exposicion = exp, control_csv = control_csv)
}

#' Lee el diccionario INEC_4 -> CIE10_4 por año y comprueba que sea único y
#' coherente con la regla numérica de inec_a_cie10().
leer_diccionario <- function(archivo) {
  dic <- fread(archivo, encoding = 'UTF-8',
               colClasses = list(character = c('CIE10_4', 'codigo_redatam')))
  if (!all(c('anio', 'INEC_4', 'CIE10_4') %in% names(dic)))
    stop('El diccionario no tiene anio, INEC_4 y CIE10_4.')
  dic[, `:=`(anio = as.integer(anio), INEC_4 = as.integer(INEC_4))]
  dic_cat <- unique(dic[, .(anio, INEC_4, CIE10_4)])
  if (anyDuplicated(dic_cat, by = c('anio', 'INEC_4')))
    stop('Un INEC_4 del mismo año tiene CIE10_4 diferentes. Revisar diccionario.')
  if (any(dic_cat$CIE10_4 != inec_a_cie10(dic_cat$INEC_4)))
    stop('Código CIE10_4 inconsistente con el código INEC_4.')
  setkey(dic_cat, anio, INEC_4)
  dic_cat
}

#' Códigos que aparecen en los Excel pero no en el diccionario de su año.
#' Verificados en los 11 Excel proporcionados; cualquier otro código sin
#' pareja detiene la corrida.
excepciones_diccionario <- function() {
  data.table(
    anio    = c(2015L, 2016L, 2024L, 2025L),
    INEC_4  = c(1009L, 1009L, 2155L, 2155L),
    CIE10_4 = c('J09', 'J09', 'U55', 'U55')
  )
}

#' Pasa las siete columnas provinciales a filas (una por provincia).
provincias_a_filas <- function(z, provincias) {
  melt(z, id.vars = c('sexo', 'edad', 'INEC_4'), measure.vars = provincias,
       variable.name = 'region', value.name = 'mortalidad',
       variable.factor = FALSE)
}

#' Lee una matriz REDATAM anual (def_AAAA.xlsx, columnas A:K).
#'
#' Controles: totales de fila contra la suma de provincias, edades válidas,
#' sexo conocido y Total de edad del Excel contra la suma de edades.
#' @return tabla anio/region/INEC_4/edad/sexo/mortalidad, con la edad 'Total'
#'   reconstruida por causa (incluye Ignorada). El bloque de sexo 'Total' del
#'   Excel se descarta para no duplicar defunciones.
leer_def <- function(archivo, anio_archivo, cfg) {
  provincias <- cfg$provincias
  sexos <- cfg$sexos
  message('Leyendo ', basename(archivo))
  x <- as.data.table(read_excel(archivo, col_names = FALSE, col_types = 'text',
                                range = cell_cols('A:K'),
                                .name_repair = 'minimal'))
  if (ncol(x) != 11L) stop('Estructura diferente de A:K en ', archivo)
  setnames(x, c('etiqueta_sexo', 'etiqueta_edad', 'codigo', provincias,
                'total_fila'))

  # En las filas sin etiqueta se mantiene el sexo y la edad del bloque anterior.
  x[, etiqueta_sexo := fifelse(grepl('^1\\. Hombres', etiqueta_sexo), 'Hombre',
                         fifelse(grepl('^2\\. Mujeres', etiqueta_sexo), 'Mujer',
                           fifelse(etiqueta_sexo == 'Total', 'Total',
                                   NA_character_)))]
  x[, sexo := arrastrar(etiqueta_sexo)]
  x[, edad_original := arrastrar(etiqueta_edad)]

  # Código de causa: SOLO filas numéricas (excluye encabezados y notas).
  x <- x[!is.na(codigo) & grepl('^[0-9]{1,4}$', trimws(codigo))]
  if (nrow(x) == 0L) stop('No se hallaron causas en ', archivo)
  x[, INEC_4 := as.integer(codigo)]
  if (anyNA(x$sexo) || anyNA(x$edad_original))
    stop('Falta sexo o edad en filas con causa, año ', anio_archivo)
  if (!all(x$sexo %in% c(sexos, 'Total')))
    stop('Etiquetas de sexo desconocidas, año ', anio_archivo)

  # Validar totales horizontales de las siete provincias contra columna K.
  x[, (provincias) := lapply(.SD, valor_num,
                             etiqueta = paste0(anio_archivo, ' provincia')),
    .SDcols = provincias]
  x[, total_col := valor_num(total_fila, paste0(anio_archivo, ' total fila'))]
  if (any(rowSums(as.matrix(x[, ..provincias])) != x$total_col))
    stop('Columnas provinciales no cuadran con Total de fila en ', anio_archivo)

  # Edades 0..130, 'Total' o 'Ignorada'; otra (p. ej. 999) detiene la corrida.
  # 90, 91, ... -> '90 años y más'.
  edad_num <- suppressWarnings(as.integer(x$edad_original))
  valido <- x$edad_original == 'Total' |
            grepl('ignor', x$edad_original, ignore.case = TRUE) |
            (!is.na(edad_num) & edad_num >= 0L & edad_num <= 130L)
  if (!all(valido))
    stop('Edades inesperadas en ', anio_archivo, ': ',
         paste(unique(x$edad_original[!valido]), collapse = ', '))
  x[, edad := fifelse(edad_original == 'Total', 'Total',
               fifelse(grepl('ignor', edad_original, ignore.case = TRUE),
                 'Ignorada', fifelse(!is.na(edad_num) & edad_num >= 90L,
                                    '90 años y más', edad_original)))]

  # Los bloques Total del Excel solo se usan para comprobaciones; no sumar.
  mujeres_hombres <- x[sexo %in% sexos]
  detalle <- provincias_a_filas(mujeres_hombres[edad != 'Total'], provincias)
  totales_excel <- provincias_a_filas(mujeres_hombres[edad == 'Total'],
                                      provincias)
  detalle[, anio := as.integer(anio_archivo)]
  totales_excel[, anio := as.integer(anio_archivo)]

  detalle <- detalle[, .(mortalidad = sum(mortalidad)),
    by = .(anio, region, INEC_4, edad, sexo)]
  totales_excel <- totales_excel[, .(mortalidad_excel = sum(mortalidad)),
    by = .(anio, region, INEC_4, sexo)]

  # Cotejar el Total del Excel (por código) frente a la suma de edades reales.
  calculados <- detalle[, .(mortalidad = sum(mortalidad)),
    by = .(anio, region, INEC_4, sexo)]
  cotejo <- merge(calculados, totales_excel,
                  by = c('anio', 'region', 'INEC_4', 'sexo'), all = TRUE)
  cotejo[is.na(mortalidad), mortalidad := 0]
  cotejo[is.na(mortalidad_excel), mortalidad_excel := 0]
  errores <- cotejo[mortalidad != mortalidad_excel]
  if (nrow(errores)) {
    print(head(errores, 20))
    stop('No coinciden los totales originales por causa/sexo/región en ',
         anio_archivo)
  }

  # Construir Total de edad, por CAUSA: incluye Ignorada; NO duplica sexo.
  totales <- copy(calculados)
  totales[, edad := 'Total']
  rbind(detalle, totales, use.names = TRUE)
}


# -----------------------------------------------------------------------------
# D. BASE POR CAUSA Y EDAD SIMPLE
# -----------------------------------------------------------------------------

#' Completa con ceros todas las edades y ambos sexos de cada causa que tuvo al
#' menos una defunción en esa provincia y año (o, si completar_ceros = FALSE,
#' conserva solo las filas observadas de esas causas).
completar_causas <- function(real, ocurridas, cfg) {
  if (cfg$completar_ceros) {
    edades <- cfg$edades
    sexos <- cfg$sexos
    # 93 edades (0..89, 90+, Ignorada, Total) x Hombre/Mujer POR causa-provincia.
    mapa <- ocurridas[, .(edad = rep(edades, each = length(sexos)),
                          sexo = rep(sexos, times = length(edades))),
                      by = .(anio, region, INEC_4)]
    d <- merge(mapa, real, by = cfg$llaves, all.x = TRUE, sort = FALSE)
    d[is.na(mortalidad), mortalidad := 0L]
  } else {
    d <- real[ocurridas, on = .(anio, region, INEC_4), nomatch = 0L]
  }
  d
}

#' Agrega CIE10_4 según año e INEC_4. Los códigos sin pareja en el
#' diccionario deben estar en 'excepciones'; si no, la corrida se detiene.
#' @return lista con la tabla 'd' y las excepciones usadas (o NULL).
asignar_cie10 <- function(d, dic_cat, excepciones, anio_actual) {
  d <- merge(d, dic_cat, by = c('anio', 'INEC_4'), all.x = TRUE, sort = FALSE)
  sin_dic <- unique(d[is.na(CIE10_4), .(anio, INEC_4)])
  usadas <- NULL
  if (nrow(sin_dic)) {
    inesperado <- sin_dic[!excepciones, on = .(anio, INEC_4)]
    if (nrow(inesperado)) {
      print(inesperado)
      stop('Códigos sin pareja no documentados en el año ', anio_actual)
    }
    usadas <- copy(sin_dic)
    d[is.na(CIE10_4), CIE10_4 := inec_a_cie10(INEC_4)]
  }
  if (anyNA(d$CIE10_4) || any(d$CIE10_4 != inec_a_cie10(d$INEC_4)))
    stop('CIE10_4 incompleta o inconsistente en ', anio_actual)
  list(d = d, excepciones = usadas)
}

#' Une la exposición y calcula mx y mx_1000, sin perder ni duplicar filas.
agregar_exposicion_y_tasas <- function(d, exp, anio_actual) {
  n_antes <- nrow(d)
  d <- merge(d, exp, by = c('edad', 'sexo', 'anio', 'region'),
             all.x = TRUE, sort = FALSE)
  if (nrow(d) != n_antes) stop('La unión con exposición multiplicó filas.')
  if (d[edad != 'Ignorada', anyNA(exposicion)])
    stop('Falta exposición para edades conocidas en el año ', anio_actual)
  d[, mx := fifelse(is.na(exposicion) | exposicion == 0,
                   NA_real_, mortalidad / exposicion)]
  d[, mx_1000 := mx * 1000]
  d
}

#' Ordena como el script quinquenal: año, provincia, causa, edad, sexo.
ordenar_base <- function(d, cfg, anio_actual) {
  d[, `:=`(orden_region = match(region, cfg$provincias),
           orden_edad = match(edad, cfg$edades),
           orden_sexo = match(sexo, cfg$sexos))]
  if (anyNA(d[, .(orden_region, orden_edad, orden_sexo)]))
    stop('Etiquetas fuera de catálogo en el año ', anio_actual)
  setorder(d, anio, orden_region, INEC_4, orden_edad, orden_sexo)
  d
}

#' Escribe 'out' en un CSV acumulativo (encabezado solo la primera vez).
anexar_csv <- function(out, archivo, ...) {
  existe <- file.exists(archivo)
  fwrite(out, archivo, append = existe, col.names = !existe, na = 'NA', ...)
}

#' Procesa un año completo: lee el Excel, completa ceros, asigna CIE-10,
#' une exposición, valida totales y escribe el CSV anual y la parte del año
#' en la base consolidada (y en la compacta, si se pidió).
#'
#' @param insumos lista con exposicion, control_csv, diccionario y excepciones.
#' @return lista con reporte (1 fila), diferencias con el CSV, top (muertes
#'   por causa y sexo) y excepciones de diccionario usadas.
procesar_anio <- function(anio_actual, cfg, insumos) {
  real <- leer_def(cfg$entrada$defunciones[[as.character(anio_actual)]],
                   anio_actual, cfg)

  # Solo se generan ceros para causas con al menos una defunción en esa misma
  # provincia y año.
  ocurridas <- real[edad == 'Total',
    .(total = sum(mortalidad)), by = .(anio, region, INEC_4)][total > 0L,
       .(anio, region, INEC_4)]

  d <- completar_causas(real, ocurridas, cfg)
  con_cie <- asignar_cie10(d, insumos$diccionario, insumos$excepciones,
                           anio_actual)
  d <- con_cie$d

  total_antes <- d[edad != 'Total', sum(mortalidad)]
  d <- agregar_exposicion_y_tasas(d, insumos$exposicion, anio_actual)

  # Todos los totales de edad se conservan, a pesar del cambio de granularidad.
  total_detalle <- d[edad != 'Total', sum(mortalidad)]
  total_filas_total <- d[edad == 'Total', sum(mortalidad)]
  if (total_antes != total_detalle || total_detalle != total_filas_total)
    stop('Se perdieron o duplicaron defunciones en ', anio_actual)
  if (d[edad == 'Ignorada' & !is.na(exposicion), .N] > 0)
    stop('La edad ignorada no debería tener exposición.')

  # Comparación NO destructiva con las muertes del CSV auxiliar: puede diferir.
  por_grupo <- d[edad != 'Total',
    .(mortalidad_excel = sum(mortalidad)), by = .(edad, sexo, anio, region)]
  control <- merge(por_grupo, insumos$control_csv[anio == anio_actual],
                   by = c('edad', 'sexo', 'anio', 'region'), all = TRUE)
  control[, diferencia := mortalidad_excel - mortalidad_csv]
  diferencias <- control[is.na(diferencia) | diferencia != 0,
    .(edad, sexo, anio, region, mortalidad_excel, mortalidad_csv, diferencia)]

  # Top 10 sin duplicar la fila de edad Total ni crear causas adicionales.
  top <- d[edad == 'Total',
    .(defunciones = sum(mortalidad)), by = .(anio, sexo, INEC_4, CIE10_4)]

  d <- ordenar_base(d, cfg, anio_actual)
  out <- d[, .(edad, sexo, anio, region, INEC_4, CIE10_4,
               mortalidad, exposicion,
               mx = fijo(mx), mx_1000 = fijo(mx_1000))]

  fwrite(out, cfg$salida$anual(anio_actual), na = 'NA', bom = FALSE)
  anexar_csv(out, cfg$salida$consolidada, bom = FALSE)
  if (cfg$guardar_version_compacta)
    anexar_csv(out[mortalidad != 0], cfg$salida$compacta, bom = FALSE)

  reporte <- data.table(
    anio = anio_actual, filas = nrow(out), causas_provincia = nrow(ocurridas),
    muertes_sin_total = total_detalle, muertes_filas_total = total_filas_total,
    ignoradas = d[edad == 'Ignorada', sum(mortalidad)],
    filas_sin_exposicion = d[is.na(exposicion), .N],
    grupos_diferentes_del_csv = nrow(diferencias)
  )
  message('  ', anio_actual, ': ', miles(nrow(out)),
          ' filas | ', miles(total_detalle),
          ' defunciones | ', nrow(diferencias), ' discrepancias con CSV')

  list(reporte = reporte, diferencias = diferencias, top = top,
       excepciones = con_cie$excepciones)
}

#' Ranking de causas: puesto y porcentaje dentro de cada grupo 'por'.
rankear_causas <- function(causas, por) {
  setorderv(causas, c(por, 'defunciones', 'INEC_4'), c(rep(1L, length(por)),
                                                       -1L, 1L))
  causas[, `:=`(puesto = seq_len(.N),
                pct = round(100 * defunciones / sum(defunciones), 1)),
         by = por]
  causas
}

#' Escribe los controles anuales, las excepciones y los dos top 10.
#' @return el resumen anual (una fila por año).
escribir_informes <- function(resultados, cfg, excepciones) {
  extraer <- function(campo) lapply(resultados, `[[`, campo)

  resumen <- rbindlist(extraer('reporte'))
  fwrite(resumen, cfg$salida$control_totales)

  comparaciones <- rbindlist(extraer('diferencias'), fill = TRUE)
  fwrite(comparaciones, cfg$salida$control_csv, na = 'NA')

  casos_excepcion <- unir_o_vacia(extraer('excepciones'),
                                  data.table(anio = integer(),
                                             INEC_4 = integer()))
  casos_excepcion <- merge(casos_excepcion, excepciones,
                           by = c('anio', 'INEC_4'), all.x = TRUE)
  fwrite(casos_excepcion, cfg$salida$excepciones)

  causas_top <- rbindlist(extraer('top'))
  total_top <- causas_top[, .(defunciones = sum(defunciones)),
                          by = .(anio, INEC_4, CIE10_4)]
  total_top <- rankear_causas(total_top, 'anio')
  fwrite(total_top[puesto <= 10,
                   .(anio, puesto, INEC_4, CIE10_4, defunciones, pct)],
         cfg$salida$top10)

  causas_top <- rankear_causas(causas_top, c('anio', 'sexo'))
  fwrite(causas_top[puesto <= 10,
                    .(anio, sexo, puesto, INEC_4, CIE10_4, defunciones, pct)],
         cfg$salida$top10_sexo)

  list(resumen = resumen, n_discrepancias = nrow(comparaciones),
       n_excepciones = nrow(casos_excepcion))
}


# -----------------------------------------------------------------------------
# E. REPARTO DE LA EDAD IGNORADA
# -----------------------------------------------------------------------------
# Método (pendiente de confirmar con el profesor; NO modificar aquí):
#  * Cada causa conserva sus muertes observadas en 'mortalidad_observada'.
#  * La referencia (base de edad simple de todas las causas, con la edad
#    ignorada ya repartida) aporta la DISTRIBUCIÓN por edad dentro de
#    sexo/año/provincia, no cifras para copiar a cada causa.
#  * Peso de cada edad = incremento de la referencia (repartida - observada)
#    entre la suma de incrementos del grupo. Si el grupo no tiene incrementos,
#    se usan las proporciones observadas por edad. Si la referencia no tiene
#    muertes observadas en ninguna edad del grupo, no hay pesos y la causa
#    conserva sus muertes en 'Ignorada'.
#  * Las discrepancias entre fuentes se registran, no se fuerzan a cero.
# -----------------------------------------------------------------------------

#' Lee la referencia y calcula los pesos por edad dentro de cada grupo
#' anio/region/sexo.
#' @return lista con 'ref' (pesos por edad) y 'pesos_grupo' (si el grupo
#'   tiene al menos un peso disponible).
preparar_pesos_referencia <- function(archivo, cfg) {
  ref_raw <- fread(archivo,
                   colClasses = list(character = c('edad', 'sexo', 'region')))
  requeridas_ref <- c('edad', 'sexo', 'anio', 'region',
                      'mortalidad_observada', 'mortalidad')
  if (!all(requeridas_ref %in% names(ref_raw)))
    stop('A la referencia le faltan: ',
         paste(setdiff(requeridas_ref, names(ref_raw)), collapse = ', '))

  ref <- ref_raw[sexo %in% cfg$sexos,
    .(edad = as.character(edad), sexo, anio = as.integer(anio), region,
      observada_ref = as.numeric(mortalidad_observada),
      repartida_ref = as.numeric(mortalidad))]

  claves_edad <- c('edad', 'sexo', 'anio', 'region')
  claves_grupo <- c('anio', 'region', 'sexo')
  if (anyDuplicated(ref, by = claves_edad))
    stop('Hay registros duplicados por edad, sexo, anio y region en referencia.')
  if (anyNA(ref[, .(observada_ref, repartida_ref)]))
    stop('La referencia contiene mortalidad NA.')
  if (!all(ref$edad %in% cfg$edades_conocidas))
    stop('La referencia tiene edades inesperadas (tras filtrar Hombre/Mujer).')

  ref[, incremento := repartida_ref - observada_ref]
  if (ref[incremento < -0.000001, .N] > 0)
    stop('La referencia presenta mortalidad repartida menor a la observada.')
  ref[abs(incremento) < 0.00000001, incremento := 0]
  ref[, `:=`(
    suma_incremento = sum(incremento),
    suma_observada = sum(observada_ref)
  ), by = claves_grupo]
  ref[, peso_edad := fifelse(
    suma_incremento > 0.00000001,
    incremento / suma_incremento,
    fifelse(suma_observada > 0, observada_ref / suma_observada, NA_real_)
  )]

  pesos_grupo <- ref[, .(peso_disponible = any(!is.na(peso_edad))),
                     by = claves_grupo]
  list(ref = ref, pesos_grupo = pesos_grupo)
}

#' Reparte la edad ignorada de cada causa en un año, a partir del CSV anual
#' escrito por procesar_anio(), y anexa el resultado a la base REPARTIDA.
#'
#' @return lista con 'control' (grupos de edad donde las fuentes difieren) y
#'   'pendientes' (ignoradas que no se pudieron repartir; NULL si no hay).
repartir_anio <- function(anio_actual, cfg, pesos) {
  claves_edad <- c('edad', 'sexo', 'anio', 'region')
  claves_causa <- c('anio', 'region', 'sexo', 'INEC_4')
  claves_grupo <- c('anio', 'region', 'sexo')
  edades_conocidas <- cfg$edades_conocidas

  archivo_anual <- cfg$salida$anual(anio_actual)
  if (!file.exists(archivo_anual)) stop('Falta archivo anual: ', archivo_anual)

  bd <- fread(archivo_anual,
              colClasses = list(character = c('edad', 'sexo', 'region',
                                              'CIE10_4')))
  if (anyDuplicated(bd, by = c(claves_causa, 'edad')))
    stop('Causas/edades duplicadas en ', anio_actual)
  setnames(bd, 'mortalidad', 'mortalidad_observada')
  bd[, mortalidad_observada := as.numeric(mortalidad_observada)]
  bd[, fila_original := .I]
  n_original <- nrow(bd)

  # LEFT JOIN por edad, sexo, anio y region (sin perder filas).
  ref_anio <- pesos$ref[anio == anio_actual,
    .(edad, sexo, anio, region, observada_ref, repartida_ref, peso_edad)]
  bd <- merge(bd, ref_anio, by = claves_edad, all.x = TRUE, sort = FALSE)
  if (nrow(bd) != n_original)
    stop('El left join altero la cantidad de filas para ', anio_actual)
  if (bd[edad %in% edades_conocidas & is.na(observada_ref), .N] > 0)
    stop('Faltan coincidencias de edad/sexo/anio/provincia para ', anio_actual)

  grupos_anio <- pesos$pesos_grupo[anio == anio_actual]
  bd[grupos_anio, peso_disponible := i.peso_disponible,
     on = .(anio, region, sexo)]

  ignoradas_causa <- bd[edad == 'Ignorada',
    .(ignorada_causa = sum(mortalidad_observada)), by = claves_causa]
  bd <- merge(bd, ignoradas_causa, by = claves_causa,
              all.x = TRUE, sort = FALSE)
  if (nrow(bd) != n_original)
    stop('La union de edades ignoradas duplico filas en ', anio_actual)
  bd[is.na(ignorada_causa), ignorada_causa := 0]

  # Repartición por causa. La referencia aporta pesos, no totales por causa.
  # La edad Total conserva el número observado de cada causa; si no hay pesos,
  # 'Ignorada' conserva las muertes no redistribuibles.
  bd[, mortalidad := mortalidad_observada]
  bd[edad %in% edades_conocidas & !is.na(peso_edad),
     mortalidad := mortalidad_observada + ignorada_causa * peso_edad]
  bd[edad == 'Ignorada' & peso_disponible == TRUE, mortalidad := 0]

  # mx y mx_1000 recalculadas con la nueva mortalidad.
  bd[, mx := fifelse(is.na(exposicion) | exposicion == 0,
                     NA_real_, mortalidad / exposicion)]
  bd[, mx_1000 := mx * 1000]

  # Control fundamental: conservar el total de defunciones POR CAUSA.
  total_causa <- bd[edad == 'Total',
    .(total_original = sum(mortalidad_observada)), by = claves_causa]
  detalle_causa <- bd[edad != 'Total',
    .(total_repartido = sum(mortalidad)), by = claves_causa]
  verificar <- merge(total_causa, detalle_causa, by = claves_causa, all = TRUE)
  verificar[, diferencia := total_repartido - total_original]
  if (anyNA(verificar[, .(total_original, total_repartido)]) ||
      verificar[abs(diferencia) > 0.0001, .N] > 0) {
    print(head(verificar[is.na(diferencia) | abs(diferencia) > 0.0001], 15))
    stop('No se conservaron los totales por causa para ', anio_actual)
  }

  # Auditoría: compara las sumas por edad de ambas fuentes sin imponer igualdad.
  chequeo <- bd[edad %in% edades_conocidas,
    .(observada_causas = sum(mortalidad_observada),
      observada_ref = observada_ref[1],
      repartida_causas = sum(mortalidad),
      repartida_ref = repartida_ref[1]),
    by = claves_edad]
  chequeo[, `:=`(
    diferencia_observada = observada_causas - observada_ref,
    diferencia_repartida = repartida_causas - repartida_ref
  )]
  control <- chequeo[abs(diferencia_observada) > 0.00001 |
                     abs(diferencia_repartida) > 0.0001]

  pendientes <- bd[edad == 'Ignorada' & mortalidad > 0,
    .(mortalidad_sin_repartir = sum(mortalidad)), by = claves_grupo]

  # Mismo orden y variables principales; mx con 10 decimales.
  setorder(bd, fila_original)
  out <- bd[, .(edad, sexo, anio, region, INEC_4, CIE10_4,
                mortalidad_observada, mortalidad, exposicion,
                mx = fijo(mx), mx_1000 = fijo(mx_1000))]
  anexar_csv(out, cfg$salida$repartida)

  message('  ', anio_actual, ': ', miles(nrow(out)),
          ' filas; ', nrow(control),
          ' grupos con diferencias respecto a la referencia.')
  list(control = control, pendientes = if (nrow(pendientes)) pendientes)
}

#' Escribe los dos controles del reparto, aunque estén vacíos.
escribir_controles_reparto <- function(resultados, cfg) {
  control <- unir_o_vacia(lapply(resultados, `[[`, 'control'),
    data.table(edad = character(), sexo = character(), anio = integer(),
               region = character(), diferencia_observada = numeric(),
               diferencia_repartida = numeric()))
  fwrite(control, cfg$salida$control_fuentes)

  sin_repartir <- unir_o_vacia(lapply(resultados, `[[`, 'pendientes'),
    data.table(anio = integer(), region = character(), sexo = character(),
               mortalidad_sin_repartir = numeric()))
  fwrite(sin_repartir, cfg$salida$sin_repartir)

  list(n_control = nrow(control), n_sin_repartir = nrow(sin_repartir),
       muertes_sin_repartir = sum(sin_repartir$mortalidad_sin_repartir))
}
